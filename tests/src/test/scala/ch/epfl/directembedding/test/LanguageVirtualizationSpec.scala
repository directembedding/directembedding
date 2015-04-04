package ch.epfl.directembedding.test

import ch.epfl.directembedding.test.Typecheck._
import ch.epfl.directembedding.test.example._
import ch.epfl.directembedding.transformers.reifyAs
import org.scalatest.{ FlatSpec, ShouldMatchers }

class LanguageVirtualizationSpec extends FlatSpec with ShouldMatchers with ExampleTester {

  /**
   * Features covered are
   */
  "dsl" should "work with a virtualized __ifThenElse" in {
    runTest(
      dsl {
        if (true) ObjectExample.valDef else ObjectExample.noArgs
      }) should be(IF(Const(true), ValDef, NoArgs))
  }

  "dsl" should "work with a virtualized __newVar" in {
    runTest(
      dsl {
        var result = "result"
        result
      }) should be(NewVar(Const("result")))
  }

  "dsl" should "work with a virtualized __whileDo" in {
    runTest(
      dsl {
        var b = "do"
        while (true) {
          b
        }
      }) should be(While(Const(true), NewVar(Const("do"))))
  }

  "dsl" should "work with a virtualized __doWhile" in {
    runTest(
      dsl {
        var b = "do"
        do {
          b
        } while (true)
      }) should be(DoWhile(Const(true), NewVar(Const("do"))))
  }

  "dsl" should "work with a virtualized __assign" in {
    runTest(
      dsl {
        var result = "initialized"
        result = "assigned"
      }) should be(Assign(NewVar(Const("initialized")), Const("assigned")))
  }

  /**
   * Virtualization of `Any` methods
   */

  "dsl" should "work with a virtualized infix_==" in {
    runTest(
      dsl {
        "true" == "false"
      }) should be(Infix_==(Const("true"), Const("false")))
  }

  "dsl" should "work with a virtualized infix_##" in {
    runTest(
      dsl {
        "true".##
      }) should be(Infix_##(Const("true")))
  }

  "dsl" should "work with a virtualized infix_equals" in {
    runTest(
      dsl {
        "true".equals("false")
      }) should be(Infix_Equals(Const("true"), Const("false")))
  }

  "dsl" should "work with a virtualized infix_hashCode" in {
    runTest(
      dsl {
        "true".hashCode
      }) should be(Infix_HashCode(Const("true")))
  }

  "dsl" should "work with a virtualized infix_isInstanceOf" in {
    runTest(
      dsl {
        "true".isInstanceOf[Int]
      }) should be(Infix_IsInstanceOf[Int](Const("true")))
  }

  "dsl" should "work with a virtualized infix_asInstanceOf" in {
    runTest(
      dsl {
        "true".asInstanceOf[Int]
      }) should be(Infix_AsInstanceOf[Int](Const("true")))
  }

  "dsl" should "work with a virtualized infix_toString" in {
    runTest(
      dsl {
        1.toString
      }) should be(Infix_ToString(Const(1)))
  }

  /**
   * Virtualization of `AnyRef` methods
   */

  "dsl" should "work with a virtualized infix_eq" in {
    runTest(
      dsl {
        "true" eq "false"
      }) should be(Infix_Eq(Const("true"), Const("false")))
  }

  "dsl" should "work with a virtualized infix_ne" in {
    runTest(
      dsl {
        "true" ne "false"
      }) should be(Infix_Ne(Const("true"), Const("false")))
  }

  "dsl" should "work with a virtualized infix_notify" in {
    runTest(
      dsl {
        "true" notify ()
      }) should be(Infix_Notify(Const("true")))
  }

  "dsl" should "work with a virtualized infix_notifyAll" in {
    runTest(
      dsl {
        "true" notifyAll ()
      }) should be(Infix_NotifyAll(Const("true")))
  }

  "dsl" should "work with a virtualized infix_wait" in {
    runTest(
      dsl {
        "true" wait ()
      }) should be(Infix_Wait(Const("true")))
  }

  "dsl" should "work with a virtualized infix_syncronized[T]" in {
    runTest(
      dsl {
        "true" synchronized 1
      }) should be(Infix_Synchronized(Const("true"), Const(1)))
  }

  /**
   * Configurable
   */

  "dsl" should "work with a virtualized __lambda(x, e)" in {
    val k = runTest(
      dsl { str: String =>
        1
      }).asInstanceOf[Infix_Lamda[String, Int]]
    k.f("") should be(Const(1))
  }

  "dsl" should "work with a virtualized __valDef" in {
    val k = runTest(
      dsl {
        val str = "true"
        str
      }) should be(Infix_ValDef(Const("true")))
  }

  "dsl" should "work with a virtualized __valDef and __ifThenElse in block" in {
    runTest(
      dsl {
        val result = if (true) ObjectExample.valDef else ObjectExample.noArgs
        result
      }) should be(Infix_ValDef(IF(true, ValDef, NoArgs)))
  }

}
