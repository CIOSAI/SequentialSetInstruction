import strutils
import std/sequtils

type
  ArithValueKind* = enum
    avNumber, avDescSet, avRealSet
  ArithValue* = ref object
    case kind*: ArithValueKind
      of avNumber: value*: float
      of avDescSet:
        paramName*: string
        description*: ArithAST
      of avRealset: items*: seq[ArithAST]
  ArithASTKind* = enum
    aaValue, aaIden, aaBoolean,
    aaCall, aaBinOp, aaUnOp, aaBoolOp, aaAssign, aaColon,
    aaLoop, aaBlock
  ArithAST* = ref object
    op*: string #aaCall, aaBinOp, aaUnOp, aaBoolOp
    operand*: ArithAST #aaCall, aaUnOp, aaAssign, aaLoop
    scope*: seq[ArithAST] #aaBlock, aaLoop
    case kind*: ArithASTKind
      of aaValue: value*: ArithValue
      of aaIden, aaAssign, aaColon: name*: string
      of aaLoop: itemName*, refName*: string
      of aaBoolean: parity*: bool
      of aaCall: 
        config*: seq[ArithAST]
      of aaUnOp, aaBlock: discard
      of aaBinOp, aaBoolOp:
        left*, right*: ArithAST

proc newASTValue*(v: ArithValue): ArithAST =
  return ArithAST(kind: aaValue, value: v)

proc newASTBoolean*(b: bool): ArithAST =
  return ArithAST(kind: aaBoolean, parity: b)

proc newASTIden*(name: string): ArithAST =
  return ArithAST(kind: aaIden, name: name)

proc newASTAssign*(name: string): ArithAST =
  return ArithAST(kind: aaAssign, name: name)

proc newASTLoop*(itemName, refName: string, v: ArithAST): ArithAST =
  return ArithAST(kind: aaLoop, itemName: itemName, refName: refName, operand: v)

proc `$`*(a: ArithAST): string

proc `$`*(v: ArithValue): string =
  case v.kind
    of avNumber: result = $v.value
    of avDescSet: result = "[with '$1': $2]" % [$v.paramName, $v.description]
    of avRealSet: result = "[" & v.items.mapIt($it).join(", ") & "]"

proc `$`*(a: ArithAST): string =
  case a.kind
    of aaValue: result = $a.value
    of aaIden: result = $a.name
    of aaBoolean: result = $a.parity
    of aaCall: result = "$1($2)" % [$a.op, concat(@[a.operand], a.config).mapIt($it).join(", ")]
    of aaBinOp: result = "($1$2$3)" % [$a.left, $a.op, $a.right]
    of aaUnOp: result = $a.op & $a.operand
    of aaBoolOp: result = "($1 $2 $3)" % [$a.left, $a.op, $a.right]
    of aaAssign: result = "$1 = $2" % [$a.name, $a.operand]
    of aaColon: result = $a.name & ":"
    of aaLoop: result = "loop($1 in $2 : $3)" % [$a.itemName, $a.operand, $a.refName]
    of aaBlock: result = "block($1)" % [$a.scope.mapIt($it).join(", ")]

proc full*(a: ArithAST): bool =
  case a.kind
    of aaBinOp, aaBoolOp:
      return a.right!=nil
    of aaUnOp, aaCall:
      return a.operand!=nil
    else:
      return false