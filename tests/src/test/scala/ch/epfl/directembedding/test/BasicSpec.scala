package ch.epfl.directembedding.test

import org.scalatest.{ FlatSpec, ShouldMatchers }
import Typecheck._
import ch.epfl.directembedding.test.example._

class BasicSpec extends FlatSpec with ShouldMatchers {

  def testReify(body: Collector => Exp[_]): Seq[Exp[_]] = {
    implicit val collec: Collector = new CollectClass()
    intercept[scala.NotImplementedError] {
      body(collec)
    }
    collec.get
  }

  "dsl" should "work object fields" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.valDef
      }) should be(List(ValDef))
  }

  "dsl" should "work with object methods without arguments and type arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.noArgs
      }) should be(List(NoArgs))
  }

  "dsl" should "work with object methods with just type arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.justTargs[TArgClassExample[Int], TArgClassExample[Int]]
      }) should be(List(JustTargs[TArgClassExample[Int], TArgClassExample[Int]]))
  }

  "dsl" should "work with object methods with just arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.justArgs(1)
      }) should be(List(JustArgs(Const(1))))
  }

  "dsl" should "work with object methods with arguments and type arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.argsAndTArgs[Int, Boolean](1, true)
      }) should be(List(ArgsAndTArgs[Int, Boolean](Const(1), Const(true))))
  }

  "dsl" should "work nested object fields" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.nested.valDef
      }) should be(List(ValDef))
  }

  "dsl" should "work with nested object methods without arguments and type arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.nested.noArgs
      }) should be(List(NoArgs))
  }

  "dsl" should "work with nested object methods with just type arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.nested.justTargs[TArgClassExample[Int], TArgClassExample[Int]]
      }) should be(List(JustTargs[TArgClassExample[Int], TArgClassExample[Int]]))
  }

  "dsl" should "work with nested object methods with just arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.nested.justArgs(1)
      }) should be(List(JustArgs(Const(1))))
  }

  "dsl" should "work with nested object methods with arguments and type arguments" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.nested.argsAndTArgs[Int, Boolean](1, true)
      }) should be(List(ArgsAndTArgs[Int, Boolean](Const(1), Const(true))))
  }

  "dsl" should "work with TArgClassExample methods with size" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].size
      }) should be(List(Size[Int](TArgClassExampleCase[Int]())))
  }

  "dsl" should "work with TArgClassExample methods with take" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].take(3)
      }) should be(List(Take[Int](TArgClassExampleCase[Int](), 3)))
  }

  "lift" should "work with TArgClassExample methods with nested take" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].take(1).take(2)
      }) should be(List(Take[Int](Take[Int](TArgClassExampleCase[Int](), Const(1)), Const(2))))
  }

  "lift" should "work with TArgClassExample methods with val x" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].x
      }) should be(List(X[Int](TArgClassExampleCase[Int]())))
  }

  "dsl" should "work with TArgClassExample methods with val y" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].y
      }) should be(List(Y[Int](TArgClassExampleCase[Int]())))
  }

  "dsl" should "work with TArgClassExample methods with method type" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Boolean].z[Int]
      }) should be(List(TArgsZ[Boolean, Int](TArgClassExampleCase[Boolean]())))
  }

  "dsl" should "work with TArgClassExample methods with app 0 args" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].app1[Int]()
      }) should be(List(AppManyArgs[Int](TArgClassExampleCase[Int])))
  }

  "dsl" should "work with TArgClassExample methods with app 1 args" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].app1[Int](1)
      }) should be(List(AppManyArgs[Int](TArgClassExampleCase[Int], 1))) // Const(1) ?
  }

  "dsl" should "work with TArgClassExample methods with app many args" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].app1[Int](1, 2)
      }) should be(List(AppManyArgs[Int](TArgClassExampleCase[Int], Const(1), Const(2))))
  }

  "dsl" should "work with construction of ClassExample" in {
    testReify(implicit collec =>
      dsl {
        new ClassExample
      }) should be(List(ClassCons))
  }

  "dsl" should "work with curry functions" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].appCurry1[Int](1)(2)
      }) should be(List(AppCurry[Int](TArgClassExampleCase[Int], 1, 2)))
  }

  "dsl" should "work with curry functions, twice" in {
    testReify(implicit collec =>
      dsl {
        new TArgClassExample[Int].appCurry2[Int](1)(2)(3)
      }) should be(List(AppCurry[Int](TArgClassExampleCase[Int], 1, 2, 3)))
  }

  "dsl" should "evaluate to the last expression in blocks" in {
    testReify(implicit collec =>
      dsl {
        ObjectExample.noArgs
        ObjectExample.valDef
      }) should be(List(ValDef))
  }

  "dsl" should "work with a virtualized boolean __ifThenElse" in {
    testReify(implicit collec =>
      dsl {
        if (true) ObjectExample.valDef else ObjectExample.noArgs
      }) should be(List(IF(Const(true), ValDef, NoArgs)))
  }

  "dsl" should "work with a virtualized boolean __ifThenElse in block" in {
    testReify(implicit collec =>
      dsl {
        val result = if (true) ObjectExample.valDef else ObjectExample.noArgs
        result
      }) should be(List(IF(true, ValDef, NoArgs)))
  }

  "dsl" should "work with a virtualized __newVar in block" in {
    testReify(implicit collec =>
      dsl {
        var result = "result"
        result
      }) should be(List(NewVar(Const("result"))))
  }

  "dsl" should "lift free variables in block" in {
    val b = true
    testReify(implicit collec =>
      dsl {
        b
      }) should be(List(Const(b)))
  }

  "dsl" should "give a useful error when a reifyAs annotation is missing" in {
    typedWithMsg(
      """
        testReify(implicit collec =>
          dsl {
            ObjectExample.missingAnnotation
        }) should be(List(???))
      """, "method missingAnnotation is not supported in example.dsl")

  }

}
