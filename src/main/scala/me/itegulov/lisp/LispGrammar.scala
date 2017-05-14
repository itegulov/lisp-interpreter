package me.itegulov.lisp

import org.parboiled2._

import scala.util.{Failure, Success}

/**
  * @author Daniyar Itegulov
  */
class LispGrammar(val input: ParserInput) extends Parser {
  def LispLine: Rule1[LispExpr] = rule { Expr ~ EOI }

  def Expr: Rule1[LispExpr] = rule {
    Condition | Definition | FunDefinition | FunCall | Atom | SExpr
  }

  def DefOrFunDef: Rule1[LispExpr] = rule { Definition | FunDefinition }

  def Atom: Rule1[LispExpr] = rule { Number | StringSymbol }

  def SExpr: Rule1[LispSExpr] = rule {
    '(' ~ zeroOrMore(DefOrFunDef ~ oneOrMore(' ')) ~ Expr ~ ')' ~>
      ((as: Seq[LispExpr], a: LispExpr) => LispSExpr.apply(as :+ a))
  }

  def Definition: Rule1[LispDefinition] = rule {
    '(' ~ "def" ~ oneOrMore(' ') ~ Symbol ~ oneOrMore(' ') ~ Expr ~ ')' ~> LispDefinition.apply _
  }

  def Condition: Rule1[LispCondition] = rule {
    '(' ~ "if" ~ oneOrMore(' ') ~ Expr ~ oneOrMore(' ') ~ Expr ~ oneOrMore(' ') ~ Expr ~ ')' ~>
      LispCondition.apply _
  }

  def FunDefinition: Rule1[LispFunDefinition] = rule {
    '(' ~ "defun" ~ oneOrMore(' ') ~ Symbol ~ oneOrMore(' ') ~
      '(' ~ Symbol ~ zeroOrMore(oneOrMore(' ') ~ Symbol) ~ ')' ~ oneOrMore(' ') ~ Expr ~ ')' ~>
      ((name: LispSymbol,
        a: LispSymbol,
        as: Seq[LispSymbol],
        e: LispExpr) => LispFunDefinition.apply(name, a +: as, e))
  }

  def FunCall: Rule1[LispFunCall] = rule {
    '(' ~ Symbol ~ oneOrMore(oneOrMore(' ') ~ Expr) ~ ')' ~> LispFunCall.apply _
  }

  def Symbol: Rule1[LispSymbol] = rule { Operator | StringSymbol }

  def StringSymbol: Rule1[LispSymbol] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~> LispSymbol.apply _
  }

  def Operator: Rule1[LispSymbol] = rule {
    (capture('+') | capture('-') | capture('*') |
      capture('/') | capture('>') | capture('<') | capture('=')) ~>
      LispSymbol.apply _
  }

  def Number: Rule1[LispNumber] = rule { capture(Digits) ~> (_.toInt) ~> LispNumber.apply _ }

  def Digits: Rule0 = rule { oneOrMore(CharPredicate.Digit) }
}

object LispGrammar {
  def parse(s: String): Either[String, LispExpr] = {
    new LispGrammar(s).LispLine.run() match {
      case Success(expr) => Right(expr)
      case Failure(e) => Left(e.getMessage)
    }
  }
}
