package ch.epfl.directembedding.test

import org.scalatest.{ FlatSpec, ShouldMatchers }

import Typecheck._
import ch.epfl.directembedding.test.example._

trait ExampleTester extends FlatSpec {
  def runTest[T](body: => T): Exp[T] = {
    intercept[scala.NotImplementedError] {
      body
    }
    Q.poll().asInstanceOf[Exp[T]]
  }

}

class BasicSpec extends FlatSpec with ShouldMatchers with ExampleTester {

  "dsl" should "work object fields" in {
    runTest[Int](
      dsl {
        ObjectExample.valDef
      }) should be(ValDef)
  }

  "dsl" should "work with object methods without arguments and type arguments" in {
    runTest(
      dsl {
        ObjectExample.noArgs
      }) should be(NoArgs)
  }

  "dsl" should "work with object methods with just type arguments" in {
    runTest(
      dsl {
        ObjectExample.justTargs[TArgClassExample[Int], TArgClassExample[Int]]
      }) should be(JustTargs[TArgClassExample[Int], TArgClassExample[Int]]())
  }

  "dsl" should "work with object methods with just arguments" in {
    runTest(
      dsl {
        ObjectExample.justArgs(1)
      }) should be(JustArgs(Const(1)))
  }

  "dsl" should "work with object methods with arguments and type arguments" in {
    runTest(
      dsl {
        ObjectExample.argsAndTArgs[Int, Boolean](1, true)
      }) should be(ArgsAndTArgs[Int, Boolean](Const(1), Const(true)))
  }

  "dsl" should "work nested object fields" in {
    runTest(
      dsl {
        ObjectExample.nested.valDef
      }) should be(ValDef)
  }

  "dsl" should "work with nested object methods without arguments and type arguments" in {
    runTest(
      dsl {
        ObjectExample.nested.noArgs
      }) should be(NoArgs)
  }

  "dsl" should "work with nested object methods with just type arguments" in {
    runTest(
      dsl {
        ObjectExample.nested.justTargs[TArgClassExample[Int], TArgClassExample[Int]]
      }) should be(JustTargs[TArgClassExample[Int], TArgClassExample[Int]])
  }

  "dsl" should "work with nested object methods with just arguments" in {
    runTest(
      dsl {
        ObjectExample.nested.justArgs(1)
      }) should be(JustArgs(Const(1)))
  }

  "dsl" should "work with nested object methods with arguments and type arguments" in {
    runTest(
      dsl {
        ObjectExample.nested.argsAndTArgs[Int, Boolean](1, true)
      }) should be(ArgsAndTArgs[Int, Boolean](Const(1), Const(true)))
  }

  "dsl" should "work with TArgClassExample methods with size" in {
    runTest(
      dsl {
        new TArgClassExample[Int].size
      }) should be(Size[Int](TArgClassExampleCase[Int]()))
  }

  "dsl" should "work with TArgClassExample methods with take" in {
    runTest(
      dsl {
        new TArgClassExample[Int].take(3)
      }) should be(Take[Int](TArgClassExampleCase[Int](), 3))
  }

  "dsl" should "work with TArgClassExample methods with nested take" in {
    runTest(
      dsl {
        new TArgClassExample[Int].take(1).take(2)
      }) should be(Take[Int](Take[Int](TArgClassExampleCase[Int](), Const(1)), Const(2)))
  }

  "dsl" should "work with TArgClassExample methods with val x" in {
    runTest(
      dsl {
        new TArgClassExample[Int].x
      }) should be(X[Int](TArgClassExampleCase[Int]()))
  }

  "dsl" should "work with TArgClassExample methods with val y" in {
    runTest(
      dsl {
        new TArgClassExample[Int].y
      }) should be(Y[Int](TArgClassExampleCase[Int]()))
  }

  "dsl" should "work with TArgClassExample methods with method type" in {
    runTest(
      dsl {
        new TArgClassExample[Boolean].z[Int]
      }) should be(TArgsZ[Boolean, Int](TArgClassExampleCase[Boolean]()))
  }

  "dsl" should "work with TArgClassExample methods with app 0 args" in {
    runTest(
      dsl {
        new TArgClassExample[Int].app1[Int]()
      }) should be(AppManyArgs[Int](TArgClassExampleCase[Int]))
  }

  "dsl" should "work with TArgClassExample methods with app 1 args" in {
    runTest(
      dsl {
        new TArgClassExample[Int].app1[Int](1)
      }) should be(AppManyArgs[Int](TArgClassExampleCase[Int], 1))
  }

  "dsl" should "work with TArgClassExample methods with app many args" in {
    runTest(
      dsl {
        new TArgClassExample[Int].app1[Int](1, 2)
      }) should be(AppManyArgs[Int](TArgClassExampleCase[Int], Const(1), Const(2)))
  }

  "dsl" should "work with construction of ClassExample" in {
    runTest(
      dsl {
        new ClassExample
      }) should be(ClassCons)
  }

  "dsl" should "work with curry functions" in {
    runTest(
      dsl {
        new TArgClassExample[Int].appCurry1[Int](1)(2)
      }) should be(AppCurry[Int](TArgClassExampleCase[Int], 1, 2))
  }

  "dsl" should "work with curry functions, twice" in {
    runTest(
      dsl {
        new TArgClassExample[Int].appCurry2[Int](1)(2)(3)
      }) should be(AppCurry[Int](TArgClassExampleCase[Int], 1, 2, 3))
  }

  "dsl" should "evaluate to the last expression in blocks" in {
    runTest(
      dsl {
        ObjectExample.noArgs
        ObjectExample.valDef
      }) should be(ValDef)
  }

  "dsl" should "lift free variables in block" in {
    val b = true
    runTest(
      dsl {
        b
      }) should be(Const(b))
  }

}
