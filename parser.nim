import npeg, strutils
import sequtils
import sss

type
  ArithTokenKind = enum
    akLParen, akRParen, akLBracket, akRBracket, akRArrow, akAssign, akEnumLoop, akColon,
    akNumber, akBoolean, akBinOp, akUnOp, akIdentifier,
    akEndMark
  ArithToken = ref object
    case kind: ArithTokenKind
      of akLParen, akRParen, akLBracket, akRBracket, 
         akRArrow, akAssign, akEndMark: discard
      of akNumber:   value: float
      of akBoolean: parity: bool
      of akBinOp, akUnOp: op: string
      of akIdentifier, akColon: name: string
      of akEnumLoop: itemName, refName: string
  TokenStream* = ref object
    stream*: seq[ArithToken]

proc initTokenStream*():TokenStream = return TokenStream(stream: @[])

proc `$`*(a: ArithToken):string =
  case a.kind
    of akNumber: result = $a.value
    of akBoolean: result = $a.parity
    of akBinOp: result = $a.op
    of akUnOp: result = $a.op
    of akIdentifier: result = $a.name
    of akLParen: result = "("
    of akRParen: result = ")"
    of akLBracket: result = "["
    of akRBracket: result = "]"
    of akRArrow: result = "->"
    of akAssign: result = "="
    of akColon: result = ":"
    of akEnumLoop: result = "enum($1 $2)" % [a.itemName, a.refName]
    of akEndMark: result = ";"

grammar "value":
  number     <- +Digit * ?("." * *Digit)
  boolean    <- "true" | "false"
  name       <- Alpha | "_" | "$"
  identifier <- +name * ! name

grammar "operator":
  unary  <- "-"|"!"
  setop  <- "-" | "&" | "|" | "^"
  boolop <- "in" | "has" | "==" | "<" | "<=" | ">=" | ">" | "<>" | "and" | "or" | "xor"

grammar "token":
  num      <- *Blank * >value.number
  boolean  <- *Blank * >value.boolean
  iden     <- *Blank * >value.identifier
  unary    <- *Blank * >operator.unary  
  binop    <- *Blank * >operator.setop
  boolop   <- *Blank * >operator.boolop
  lparen   <- *Blank * '('
  rparen   <- *Blank * ')'
  lbrack   <- *Blank * '['
  rbrack   <- *Blank * ']'
  rarrow   <- *Blank * "->"
  colon    <- *Blank * ':'
  assign   <- *Blank * '='
  delim    <- *Blank * ','
  enumloop <- *Blank * "enum"

let lexExp* = peg("exp", st: TokenStream):
  exp          <- factor * *(call | binoperation):
    st.stream.add ArithToken(kind: akEndMark)
  booltail     <- boolop * exp
  binoperation <- binop * factor
  call         <- rarrow * iden * ?config
  config       <- lparen * exp * listtail * rparen
  factor       <- ?unary * ( value|(lparen * exp * rparen) )
  genset       <- lbrack * ?(
                  boolean |
                  (exp * ?(colon * exp) * ( booltail | listtail ))
                ) * rbrack
  listtail     <- *(token.delim * exp) * ?token.delim
  value        <- num|iden|genset
  
  rarrow  <- token.rarrow:  st.stream.add ArithToken(kind: akRArrow)
  colon   <- token.colon:   st.stream.add ArithToken(kind: akColon)
  lparen  <- token.lparen:  st.stream.add ArithToken(kind: akLParen)
  rparen  <- token.rparen:  st.stream.add ArithToken(kind: akRParen)
  lbrack  <- token.lbrack:  st.stream.add ArithToken(kind: akLBracket)
  rbrack  <- token.rbrack:  st.stream.add ArithToken(kind: akRBracket)
  boolop  <- token.boolop:  st.stream.add ArithToken(kind: akBinOp, op: ($1))
  binop   <- token.binop:   st.stream.add ArithToken(kind: akBinOp, op: ($1))
  unary   <- token.unary:   st.stream.add ArithToken(kind: akUnOp, op: ($1))
  num     <- token.num:     st.stream.add ArithToken(kind: akNumber, value: parseFloat($1))
  boolean <- token.boolean: st.stream.add ArithToken(kind: akBoolean, parity: $1=="true")
  iden    <- token.iden:    st.stream.add ArithToken(kind: akIdentifier, name: $1)

let lexSta* = peg("assignment", st: TokenStream):
  assignment <- token.iden * token.assign:
    st.stream.add ArithToken(kind: akIdentifier, name: $1)
    st.stream.add ArithToken(kind: akAssign)

let lexLoop* = peg("loop", st: TokenStream):
  loop <- token.enumloop * token.lparen * >token.iden * ?(token.delim  * >token.iden) * token.rparen:
    if capture.len>=5:
      st.stream.add ArithToken(kind: akEnumLoop, itemName: $2, refName: $4)
    else:
      st.stream.add ArithToken(kind: akEnumLoop, itemName: $2, refName: "")
    st.stream.add ArithToken(kind: akEndMark)

proc parser*(tokens: seq[ArithToken]):tuple[ast: ArithAST, eaten: int] =
  var stack: seq[ArithAST]
  var pos = 0

  proc completeLast() =
    let leaf = stack.pop
    let branch = stack.pop
    case branch.kind
      of aaBinOp, aaBoolOp: branch.right = leaf
      of aaUnOp, aaCall, aaAssign: branch.operand = leaf
      else:
        raise newException(Exception, "unexpected branch: " & $branch.kind)
    stack.add branch
  proc attemptPop() =
    if stack.len<2: return
    if stack[^2].kind notin [aaBinOp, aaBoolOp, aaUnOp, aaCall, aaAssign]: return
    if stack[^2].full: return
    completeLast()
    attemptPop()
  while pos<tokens.len:
    let t = tokens[pos]
    case t.kind:
      of akNumber:
        stack.add newASTValue(ArithValue(
          kind: avNumber, value: t.value
        ))
        pos.inc
      of akBoolean:
        stack.add newASTBoolean(t.parity)
        pos.inc
      of akIdentifier:
        stack.add newASTIden(t.name)
        pos.inc
      of akBinOp:
        let isSetOp = patt operator.setop
        if isSetOp.match(t.op).ok:
          stack.add ArithAST(kind: aaBinOp, op: t.op, left: stack.pop)
        else:
          stack.add ArithAST(kind: aaBoolOp, op: t.op, left: stack.pop)
        pos.inc
      of akAssign:
        stack.add newASTAssign(stack.pop().name)
        pos.inc
      of akColon:
        let prev = stack.pop()
        if prev.kind == aaIden:
          stack.add ArithAST(kind: aaColon, name: prev.name)
        else:
          raise newException(Exception, "unexpected colon")
        pos.inc
      of akEnumLoop:
        stack.add newASTLoop(t.itemName, t.refName, stack.pop)
        pos.inc
      of akUnOp:
        stack.add ArithAST(kind: aaUnOp, op: t.op)
        pos.inc
      of akRArrow:
        pos.inc
        stack.add ArithAST(kind: aaCall, op: tokens[pos].name, operand: stack.pop, config: @[])
        pos.inc
      of akLBracket:
        let inner = parser(tokens[pos+1..^1])
        stack.add inner.ast
        pos += inner.eaten
        pos.inc
      of akRBracket:
        result.eaten = pos+1
        if stack.len>0:
          if stack[^1].kind in [aaBoolOp, aaBoolean]:
            if tokens[0].kind == akIdentifier:
              result.ast = newASTValue( 
                ArithValue(kind: avDescSet, paramName: tokens[0].name, description: stack.pop)
              )
            elif stack[0].kind == aaColon:
              result.ast = newASTValue( 
                ArithValue(kind: avDescSet, paramName: stack[0].name, description: stack.pop)
              )
            else:
              echo "error: description set can't find param name"
              result.ast = newASTValue( 
                ArithValue(kind: avDescSet, paramName: "", description: stack.pop)
              )
          else:
            result.ast = newASTValue( 
              ArithValue(kind: avRealSet, items: stack)
            )
        else:
          result.ast = newASTValue( 
            ArithValue(kind: avRealSet, items: @[])
          )
        return result
      of akLParen:
        let inner = parser(tokens[pos+1..^1])
        
        proc isCall():bool =
          if stack.len==0: return false
          if stack[^1].kind!=aaCall: return false
          return true
        
        if isCall():
          stack[^1].config = inner.ast.config
        else:
          stack.add inner.ast.config[0]
        
        pos += inner.eaten
        pos.inc
      of akRParen: 
        return (ArithAST(kind: aaCall, config: stack), pos+1)
      of akEndMark:
        pos.inc
        attemptPop()
      else:
        break
  return (stack.pop, stack.len)