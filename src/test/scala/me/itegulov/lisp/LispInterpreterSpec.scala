package me.itegulov.lisp

import org.scalatest._


class LispInterpreterSpec extends FlatSpec with Matchers with EitherValues {
  def test(s: String): Either[String, LispAtom] = {
    for {
      lispExpr <- LispGrammar.parse(s)
      lispAtomWithContext <- LispInterpreter.interpret(lispExpr)
      (lispAtom, _) = lispAtomWithContext
    } yield lispAtom
  }
  
  "Interpreter" should "work on example 1 from Stepik" in {
    val result = test("((def pi 3) (defun area (x) (* pi (* x x))) (area 10))")
    result.right.value should be (LispNumber(300))
  }

  "Interpreter" should "work on example 2 from Stepik" in {
    val result = test("(list 1 2 3)")
    result.right.value should be (LispList(Seq(LispNumber(1), LispNumber(2), LispNumber(3))))
  }

  "Interpreter" should "work on example 3 from Stepik" in {
    val result = test("((defun fib (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))) (fib 16))")
    result.right.value should be (LispNumber(1597))
  }

  "Interpreter" should "work on example 4 from Stepik" in {
    val result = test("(car (cdr (list 1 2 3)))")
    result.right.value should be (LispNumber(2))
  }
}
