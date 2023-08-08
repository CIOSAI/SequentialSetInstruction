import npeg
import sss
import parser
import interpreter
import sequtils
import strutils

const debug = false

proc exceptEmpty(source: seq[string]):seq[string] =
  for line in source:
    let isComment = patt *(Blank | Space) * "#" * *1
    let allBlank = patt *(Blank | Space) * !1
    if isComment.match(line).ok: continue
    if allBlank.match(line).ok: continue
    result.add line

proc parseLine(source: string):tuple[ast: ArithAST, tabs: int] =
  var tabsPeg = patt *"    " * !' '
  let tabsCheck = tabsPeg.match(source)
  if tabsCheck.ok:
    result.tabs = tabsCheck.matchLen div 4
  else:
    raise newException(Exception, "spaces before line \"$1\" isn't exactly multiples of 4" % source)

  var ts = initTokenStream()

  let assignCheck = lexSta.match(source, ts)
  if assignCheck.ok:
    if lexExp.match(source[assignCheck.matchLen..^1], ts).ok:
      if debug: echo "parse success"
  else:
    let expression = lexExp.match(source, ts)
    if expression.ok:
      if lexLoop.match(source[expression.matchLen..^1], ts).ok:
        if debug: echo "loop parse success"
      else:
        if debug: echo "loop parse fail"
      
      if debug: echo "parse success"

  result.ast = parser(ts.stream).ast
  if debug: echo result

proc makeTree(codeLines: seq[tuple[ast: ArithAST, tabs: int]]): tuple[ast: ArithAST, eaten: int] =
  var stack: seq[ArithAST]
  var pos = 0
  while pos<codeLines.len:
    let line = codeLines[pos]
    
    if line.tabs<0:
      break
    elif line.tabs>0:
      let inner = makeTree(codeLines[pos..^1].mapIt( (it.ast, it.tabs-line.tabs) ))
      if stack[^1].kind == aaLoop:
        stack[^1].scope = inner.ast.scope
      else:
        stack.add inner.ast
      pos += inner.eaten
    else:
      stack.add line.ast
      pos.inc
  return (ast: ArithAST(kind: aaBlock, scope: stack), eaten: pos)

proc runSss*(sourceCode: string):Interpretation {.exportc.} =
  let context = newInterpreter()
  let (mainBlock, _) = sourceCode
    .split("\n")
    .exceptEmpty()
    .mapIt(it.parseLine())
    .makeTree()
  return context.interpret(mainBlock)