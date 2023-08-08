import hashes, sets, strutils, tables
import std/sequtils
import sss

type
  InterpretationKind = enum
    ikNumber, ikRealSet, ikDescSet, ikBoolean
  Interpretation* = ref object
    case kind: InterpretationKind
      of ikNumber: value: float
      of ikDescSet: 
        paramName: string
        description: ArithAST
      of ikRealSet: items: HashSet[Interpretation]
      of ikBoolean: parity: bool
  Context = Table[string, Interpretation]
  ContextStack = seq[Context]
  Interpreter = ref object
    context: ContextStack

proc `==`*(a: Interpretation, b: Interpretation): bool=
  case a.kind
    of ikNumber:
      case b.kind
        of ikNumber:
          return a.value == b.value
        else:
          return false
    of ikRealSet:
      case b.kind
        of ikRealSet:
          if a.items.len != b.items.len: return false
          else:
            for i in 0..<a.items.len:
              if a.items.toSeq()[i] != b.items.toSeq()[i]: return false
            return true
        else:
          return false
    of ikDescSet:
      case b.kind
        of ikDescSet:
          return a.description == b.description
        else:
          return false
    of ikBoolean:
      case b.kind
        of ikBoolean:
          return a.parity == b.parity
        else:
          return false

proc hash*(a: Interpretation): Hash=
  case a.kind
    of ikNumber:
      return hash(a.value)
    of ikRealSet:
      return hash(a.items)
    of ikDescSet:
      return hash($a.description)
    of ikBoolean:
      return hash(a.parity)

proc push(ctx: var ContextStack, toAdd: Context)=
  ctx.add(toAdd)

proc pop(ctx: var ContextStack)=
  ctx = ctx[0..^2]

proc newInterpreter*(): Interpreter=
  result = Interpreter(context: @[])

proc interpret*(interpreter: Interpreter, node: ArithAST): Interpretation

proc `$`*(v: Interpretation): string=
  case v.kind:
    of ikNumber:
      return $v.value
    of ikRealSet:
      return "{" & v.items.mapIt($it).join(", ") & "}"
    of ikDescSet:
      return $v.description
    of ikBoolean:
      return $v.parity

proc coerceToSet(v: Interpretation): Interpretation=
  case v.kind:
    of ikNumber:
      return Interpretation(kind: ikRealSet, items: [Interpretation(kind: ikNumber, value: v.value)].toHashSet)
    else:
      return v

proc coerceToDescSet(v: Interpretation): Interpretation=
  let asNumNodes = v.items.mapIt(newASTValue(ArithValue(
    kind: avNumber, value: it.value
  )))
  let asRealSetNode = newASTValue(ArithValue(kind: avRealSet, 
      items: asNumNodes
  ))
  let asDescNode = ArithAST(kind: aaBoolOp, op: "in", left: newASTIden("$"), 
    right: asRealSetNode
  )
  return Interpretation(kind: ikDescSet, description: asDescNode)

proc coerceToDescSet(l: Interpretation, r: Interpretation): 
  tuple[l: Interpretation, r: Interpretation]=
  if l.kind == ikRealSet:
    if r.kind == ikRealSet:
      echo "unnecessary coersion of ", $l, " and ", $r, " to real sets"
      return (l, r)
    else:
      return (coerceToDescSet(l), r)
  else:
    if r.kind == ikRealSet:
      return (l, coerceToDescSet(r))
    else:
      return (l, r)

template setRealize(
  interpreter: Interpreter, 
  target: Interpretation, desc: Interpretation, 
  dumpTo: var HashSet[Interpretation])=
  if desc.description.kind == aaBoolOp:
    for i in target.items:
      interpreter.context.push({
        desc.paramName: i
      }.toTable)
      if interpreter.interpret(desc.description).parity:
        dumpTo.incl(i)
      interpreter.context.pop()
  else:
    if desc.description.value.description.parity:
      dumpTo = target.items
    else:
      dumpTo.clear()

proc binop(interpreter: Interpreter, name: string): proc(l: Interpretation, r:Interpretation): Interpretation=
  case name:
    of "-":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikRealSet and r.kind == ikRealSet:
          return Interpretation(kind: ikRealSet, items: l.items - r.items)
        else:
          let (left, right) = coerceToDescSet(l, r)
          return Interpretation(kind: ikDescSet, 
            description: ArithAST(kind: aaBoolOp, op: "and", 
              left: left.description, 
              right: ArithAST(kind: aaUnOp, op: "!", operand: right.description)
            )
          )
    of "&": 
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikRealSet:
          if r.kind == ikRealSet:
            return Interpretation(kind: ikRealSet, items: l.items * r.items)
          else:
            var dump = HashSet[Interpretation]()
            setRealize(interpreter, l, r, dump)
            return Interpretation(kind: ikRealSet, items: dump)
        else:
          if r.kind == ikRealSet:
            var dump = HashSet[Interpretation]()
            setRealize(interpreter, r, l, dump)
            return Interpretation(kind: ikRealSet, items: dump)
          else:
            return Interpretation(kind: ikDescSet, 
              description: ArithAST(kind: aaBoolOp, op: "and", left: l.description, right: r.description)
            )
    of "|":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikRealSet and r.kind == ikRealSet:
          return Interpretation(kind: ikRealSet, items: l.items + r.items)
        else:
          let (left, right) = coerceToDescSet(l, r)
          return Interpretation(kind: ikDescSet, 
            description: ArithAST(kind: aaBoolOp, op: "or", left: left.description, right: right.description)
          )
    of "^":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikRealSet and r.kind == ikRealSet:
          return Interpretation(kind: ikRealSet, items: l.items -+- r.items)
        else:
          let (left, right) = coerceToDescSet(l, r)
          return Interpretation(kind: ikDescSet, 
            description: ArithAST(kind: aaBoolOp, op: "xor", left: left.description, right: right.description)
          )
    else:
      raise newException(Exception, "unexpected operator: " & name)

proc unaryop(interpreter: Interpreter, name: string): proc(operand: Interpretation): Interpretation=
  case name:
    of "!":
      return proc(operand: Interpretation): Interpretation= 
        if operand.kind == ikBoolean:
          return Interpretation(kind: ikBoolean, parity: not operand.parity)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    of "-":
      return proc(operand: Interpretation): Interpretation= 
        case operand.kind:
          of ikNumber:
            return Interpretation(kind: ikNumber, value: -operand.value)
          of ikRealSet:
            var remap:HashSet[Interpretation]
            for i in operand.items:
              if i.kind == InterpretationKind.ikNumber:
                remap.incl(Interpretation(kind: ikNumber, value: -i.value))
              else:
                raise newException(Exception, "- cannot be applied to a set of non-numbers")
            return Interpretation(kind: ikRealSet, items: remap)
          else:
            raise newException(Exception, "unexpected operand type for operator: " & name)
    else:
      raise newException(Exception, "unexpected operator: " & name)

proc boolop(interpreter: Interpreter, name: string): proc(l: Interpretation, r:Interpretation): Interpretation=
  case name:
    of "==":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        return Interpretation(kind: ikBoolean, parity: l == r)
    of "<>": 
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        return Interpretation(kind: ikBoolean, parity: l != r)
    of "<":
      return proc(l: Interpretation, r:Interpretation): Interpretation=
        if l.kind == ikNumber and r.kind == ikNumber:
          return Interpretation(kind: ikBoolean, parity: l.value < r.value)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    of ">":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikNumber and r.kind == ikNumber:
          return Interpretation(kind: ikBoolean, parity: l.value > r.value)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    of "<=":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikNumber and r.kind == ikNumber:
          return Interpretation(kind: ikBoolean, parity: l.value <= r.value)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    of ">=":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikNumber and r.kind == ikNumber:
          return Interpretation(kind: ikBoolean, parity: l.value >= r.value)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    of "in":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        case r.kind:
          of ikRealSet:
            return Interpretation(kind: ikBoolean, parity: r.items.contains(l))
          of ikDescSet:
            if r.description.kind == aaBoolOp:
              interpreter.context.push({
                r.paramName: l
              }.toTable)
              interpreter.context.pop()
              return Interpretation(kind: ikBoolean, parity: interpreter.interpret(r.description).parity)
            else:
              Interpretation(kind: ikBoolean, parity: r.description.value.description.parity)
          else:
            raise newException(Exception, "unexpected operand type for operator: " & name)
    of "has":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        case l.kind:
          of ikRealSet:
            return Interpretation(kind: ikBoolean, parity: l.items.contains(r))
          of ikDescSet:
            if l.description.kind == aaBoolOp:
              interpreter.context.push({
                l.paramName: r
              }.toTable)
              interpreter.context.pop()
              return Interpretation(kind: ikBoolean, parity: interpreter.interpret(l.description).parity)
            else:
              Interpretation(kind: ikBoolean, parity: l.description.value.description.parity)
          else:
            raise newException(Exception, "unexpected operand type for operator: " & name)
    of "and":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikBoolean and r.kind == ikBoolean:
          return Interpretation(kind: ikBoolean, parity: l.parity and r.parity)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    of "or":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikBoolean and r.kind == ikBoolean:
          return Interpretation(kind: ikBoolean, parity: l.parity or r.parity)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    of "xor":
      return proc(l: Interpretation, r:Interpretation): Interpretation= 
        if l.kind == ikBoolean and r.kind == ikBoolean:
          return Interpretation(kind: ikBoolean, parity: l.parity xor r.parity)
        else:
          raise newException(Exception, "unexpected operand type for operator: " & name)
    else:
      raise newException(Exception, "unexpected operator: " & name)

proc funcop(interpreter: Interpreter, name: string): 
  proc(operand: Interpretation, config:seq[Interpretation]): Interpretation=

  case name:
    of "sum":
      return proc(operand: Interpretation, config:seq[Interpretation]): Interpretation= 
        var target = operand.coerceToSet()
        if target.kind == ikDescSet:
          raise newException(Exception, "sum() cannot be applied to a description set")
        else:
          var num:float = 0.0
          for i in target.items:
            if i.kind == ikNumber:
              num += i.value
            else:
              raise newException(Exception, "sum() cannot be applied to a set of non-numbers")
          return Interpretation(kind: ikNumber, value: num)
    of "mod":
      return proc(operand: Interpretation, config:seq[Interpretation]): Interpretation= 
        case operand.kind:
          of ikNumber:
            return Interpretation(kind: ikNumber, value: float(int(operand.value) mod int(config[0].value)))
          of ikRealSet:
            var collect:seq[Interpretation]
            for i in operand.items:
              if i.kind == ikNumber:
                collect.add Interpretation(kind: ikNumber, value: float(int(i.value) mod int(config[0].value)))
              else:
                raise newException(Exception, "mod() can only be applied to a set of just numbers or a single number")
            return Interpretation(kind: ikRealSet, items: collect.toHashSet)
          else:
            raise newException(Exception, "mod() can only be applied to a set of just numbers or a single number")
    of "comb":
      return proc(operand: Interpretation, config:seq[Interpretation]): Interpretation= 
        var target = operand.coerceToSet()
        if target.kind == ikDescSet:
          raise newException(Exception, "comb() cannot be applied to a description set")
        else:
          if config[0].kind != ikNumber:
            raise newException(Exception, "comb() requires a number as the first argument")
          else:
            # https://forum.nim-lang.org/t/2812
            proc repeatedPermutations[T](a: openarray[T], n: int): seq[seq[T]] =
              result = newSeq[seq[T]]()
              if n <= 0: return
              for i in 0 .. a.high:
                if n == 1:
                  result.add(@[a[i]])
                else:
                  for j in repeatedPermutations(a, n - 1):
                    result.add(a[i] & j)
            
            var combinations:HashSet[Interpretation]
            for i in repeatedPermutations(target.items.toSeq(), config[0].value.toInt):
              combinations.incl(Interpretation(kind: ikRealSet, items: i.toHashSet()))
            return Interpretation(kind: ikRealSet, items: combinations)
    of "collapse":
      return proc(operand: Interpretation, config:seq[Interpretation]): Interpretation= 
        case operand.kind:
          of ikDescSet:
            raise newException(Exception, "collapse() cannot be applied to a description set")
          of ikRealSet:
            if operand.items.len != 1:
              return operand
            else:
              var target = operand
              while target.items.len == 1:
                target = target.items.toSeq()[0]
              return target
          else:
            return operand
    of "print":
      return proc(operand: Interpretation, config:seq[Interpretation]): Interpretation= 
        echo operand
        return Interpretation(kind: ikRealSet, items: initHashSet[Interpretation]())
    else:
      raise newException(Exception, "unexpected function: " & name)

proc interpret*(interpreter: Interpreter, node: ArithAST): Interpretation=
  # echo node.kind
  case node.kind:
    of aaBlock:
      interpreter.context.push(initTable[string, Interpretation]())
      for line in node.scope:
        discard interpreter.interpret(line)
      interpreter.context.pop()
    of aaLoop:
      let loopOver = interpreter.interpret(node.operand).coerceToSet
      if loopOver.kind == ikDescSet:
        raise newException(Exception, "loop cannot be applied to a description set")
      
      interpreter.context.push({
        node.itemName: Interpretation(kind: ikRealSet, items: initHashSet[Interpretation]()),
        (if node.refName=="": "<loop>" else: node.refName): loopOver
      }.toTable)

      proc getLooper():Interpretation =
        return interpreter.context[^1].getOrDefault((if node.refName=="": "<loop>" else: node.refName), 
          Interpretation(kind: ikRealSet, items: initHashSet[Interpretation]())
        )
      
      while getLooper().items.len > 0:
        interpreter.context[^1][node.itemName] = getLooper().items.pop()
        for line in node.scope:
          discard interpreter.interpret(line)
      
      interpreter.context.pop()
    of aaBinOp:
      let l = interpreter.interpret(node.left).coerceToSet
      let r = interpreter.interpret(node.right).coerceToSet
      let op = interpreter.binop(node.op)
      return op(l, r)
    of aaUnOp:
      let operand = interpreter.interpret(node.operand)
      let op = interpreter.unaryop(node.op)
      return op(operand)
    of aaBoolOp:
      let l = interpreter.interpret(node.left)
      let r = interpreter.interpret(node.right)
      let op = interpreter.boolop(node.op)
      return op(l, r)
    of aaValue:
      let v = node.value
      case v.kind
        of avNumber:
          return Interpretation(kind: ikNumber, value: v.value)
        of avRealSet:
          return Interpretation(kind: ikRealSet, 
            items: v.items.mapIt(interpreter.interpret(it)).toHashSet
          )
        of avDescSet:
          return Interpretation(kind: ikDescSet, paramName: v.paramName, description: v.description)
    of aaCall:
      let operand = interpreter.interpret(node.operand)
      let configs = node.config.mapIt(interpreter.interpret(it))
      let op = interpreter.funcop(node.op)
      return op(operand, configs)
    of aaIden:
      var searchFrom = interpreter.context.len - 1
      var last = interpreter.context[searchFrom]
      while last.hasKey(node.name) == false and searchFrom > 0:
        searchFrom -= 1
        last = interpreter.context[searchFrom]
      if not last.hasKey(node.name):
        raise newException(Exception, "undefined variable: " & node.name)
      return last.getOrDefault(node.name, Interpretation(kind: ikRealSet, items: initHashSet[Interpretation]()))
    of aaAssign:
      let value = interpreter.interpret(node.operand)
      interpreter.context[^1][node.name] = value
      return Interpretation(kind: ikRealSet, items: initHashSet[Interpretation]())
    else:
      echo "unrecognized AST node: " & $node