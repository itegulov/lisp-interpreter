package me.itegulov.lisp

import cats.implicits._

object LispInterpreter {
  def interpret(
    lispExpr: LispExpr,
    context: LispContext = LispContext.standard
  ): Either[String, (LispAtom, LispContext)] =
    lispExpr match {
      case LispSExpr(exprs) =>
        var lContext = context
        for (expr <- exprs.init) {
          interpret(expr, lContext) match {
            case Right((_, newContext)) =>
              lContext = newContext
            case left => return left
          }
        }
        interpret(exprs.last, lContext)
      case LispDefinition(symbolName, expr) =>
        for {
          atomWithContext <- interpret(expr, context)
          (atom, _) = atomWithContext
        } yield (atom, context.addDef(symbolName -> atom))
      case fun @ LispFunDefinition(funName, _, _) =>
        Right((funName, context.addFunction(funName -> fun)))
      case LispCondition(conditionExpr, thenExpr, elseExpr) =>
        val result = interpret(conditionExpr, context)
        result match {
          case Right((LispNumber(1), _)) =>
            interpret(thenExpr, context)
          case Right((LispNumber(0), _)) =>
            interpret(elseExpr, context)
          case left: Left[String, (LispAtom, LispContext)] =>
            left
          case _ =>
            Left("Invalid conditional expression")
        }
      case LispFunCall(funName, arguments) =>
        context.functionContext.get(funName) match {
          case Some(LispFunDefinition(_, argNames, expr)) =>
            if (argNames.length == arguments.length) {
              for {
                argValuesWithContext <- arguments.map(interpret(_, context)).toList.sequenceU
                (argValues, _) = argValuesWithContext.unzip
                result <- interpret(expr, context.addDefs(argNames.zip(argValues)))
              } yield result
            } else {
              Left(s"Required ${argNames.length} arguments for function ${funName.sym}, but got ${arguments.length}"
              )
            }
          case Some(LispNativeFunction(_, function)) =>
            for {
              args <- arguments.map(interpret(_, context)).toList.sequenceU
              (argValues, _) = args.unzip
            } yield (function(argValues), context)
          case None =>
            Left(s"Unknown function ${funName.sym}")
        }
      case symbol: LispSymbol =>
        context.defContext.get(symbol) match {
          case Some(value) => Right((value, context))
          case None        => Left(s"Unknown symbol ${symbol.sym}")
        }
      case atom: LispAtom                 => Right((atom, context))
      case LispNativeFunction(funName, _) => Right((funName, context))
    }
}
