package me.itegulov.lisp

import scala.language.implicitConversions

case class LispContext private (functionContext: Map[LispSymbol, LispFun],
                                defContext: Map[LispSymbol, LispAtom]) {
  def addFunction(nameAndFunction: (LispSymbol, LispFun)): LispContext =
    LispContext(functionContext + nameAndFunction, defContext)
  def addDef(nameAndAtom: (LispSymbol, LispAtom)): LispContext =
    LispContext(functionContext, defContext + nameAndAtom)
  def addDefs(nameAndAtoms: Seq[(LispSymbol, LispAtom)]): LispContext =
    LispContext(functionContext, defContext ++ nameAndAtoms)
}

object LispContext {
  private implicit def stringToLispSym(s: String): LispSymbol        = LispSymbol(s)
  private implicit def intToLispNum(i: Int): LispNumber              = LispNumber(i)
  private implicit def listToLispList(list: Seq[LispAtom]): LispList = LispList(list)

  private val nativeIntFunctions = Map(
    LispSymbol("+") ->
      LispNativeFunction("+", atoms => atoms.map(_.asInstanceOf[LispNumber].num).sum),
    LispSymbol("-") ->
      LispNativeFunction("-", atoms => {
        val numbers = atoms.map(_.asInstanceOf[LispNumber].num)
        numbers.head - numbers.tail.sum
      }),
    LispSymbol("*") ->
      LispNativeFunction("*", atoms => atoms.map(_.asInstanceOf[LispNumber].num).product),
    LispSymbol("/") ->
      LispNativeFunction("/", atoms => {
        val numbers = atoms.map(_.asInstanceOf[LispNumber].num)
        numbers.head / numbers.tail.product
      }),
    LispSymbol("max") ->
      LispNativeFunction("max", atoms => atoms.map(_.asInstanceOf[LispNumber].num).max),
    LispSymbol("min") ->
      LispNativeFunction("min", atoms => atoms.map(_.asInstanceOf[LispNumber].num).min),
    LispSymbol(">") ->
      LispNativeFunction(">", atoms => {
        val numbers = atoms.map(_.asInstanceOf[LispNumber].num)
        if (numbers.zip(numbers.tail).forall { case (x, y) => x > y }) 1 else 0
      }),
    LispSymbol("<") ->
      LispNativeFunction(">", atoms => {
        val numbers = atoms.map(_.asInstanceOf[LispNumber].num)
        if (numbers.zip(numbers.tail).forall { case (x, y) => x < y }) 1 else 0
      }),
    LispSymbol("=") ->
      LispNativeFunction(">", atoms => {
        val numbers = atoms.map(_.asInstanceOf[LispNumber].num)
        if (numbers.forall(_ == numbers.head)) 1 else 0
      })
  )

  private val nativeListFunctions = Map(
    LispSymbol("list") ->
      LispNativeFunction("list", atoms => LispList(atoms)),
    LispSymbol("list-length") ->
      LispNativeFunction(
        "list-length",
        atoms => atoms.head.asInstanceOf[LispList].elements.length
      ),
    LispSymbol("car") ->
      LispNativeFunction("car", atoms => atoms.head.asInstanceOf[LispList].elements.head),
    LispSymbol("cdr") ->
      LispNativeFunction("cdr", atoms => atoms.head.asInstanceOf[LispList].elements.tail)
  )

  def standard: LispContext = LispContext(
    nativeIntFunctions ++ nativeListFunctions,
    Map.empty
  )
}
