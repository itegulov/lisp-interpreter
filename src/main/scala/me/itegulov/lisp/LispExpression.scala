package me.itegulov.lisp

sealed trait LispExpr

final case class LispSExpr(exprs: Seq[LispExpr]) extends LispExpr

final case class LispDefinition(symbolName: LispSymbol, expr: LispExpr) extends LispExpr

final case class LispCondition(conditionExpr: LispExpr, thenExpr: LispExpr, elseExpr: LispExpr) extends LispExpr

sealed trait LispFun extends LispExpr

final case class LispFunDefinition(funName: LispSymbol, argNames: Seq[LispSymbol], expr: LispExpr) extends LispFun

final case class LispNativeFunction(funName: LispSymbol, function: Seq[LispAtom] => LispAtom) extends LispFun

final case class LispFunCall(funName: LispSymbol, arguments: Seq[LispExpr]) extends LispExpr

sealed trait LispAtom extends LispExpr

final case class LispNumber(num: Int) extends LispAtom

final case class LispSymbol(sym: String) extends LispAtom

final case class LispList(elements: Seq[LispAtom]) extends LispAtom
