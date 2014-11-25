package ch.epfl.directembedding.test

import org.scalatest.{ FlatSpec, ShouldMatchers }

class BasicSpec extends FlatSpec with ShouldMatchers {

  def testReify(body: Collector => Exp[_]): Seq[Exp[_]] = {
    implicit val collec: Collector = new CollectClass()
    intercept[scala.NotImplementedError] {
      body(collec)
    }
    collec.get
  }

  "lift" should "work object fields" in {
    testReify(implicit collec =>
      lift {
        ObjectExample.valDef
      }) should be(List(ValDef))
  }

  "lift" should "work with object methods without arguments and type arguments" in {
    testReify(implicit collec =>
      lift {
        ObjectExample.noArgs
      }) should be(List(NoArgs))
  }

  "lift" should "work with object methods with just type arguments" in {
    testReify(implicit collec =>
      lift {
        ObjectExample.justTArgs[TArgClassExample[Int], TArgClassExample[Int]]
      }) should be(List(JustTArgs[TArgClassExample[Int], TArgClassExample[Int]]))
  }

  "lift" should "work with object methods with just arguments" in {
    testReify(implicit collec =>
      lift {
        ObjectExample.justArgs(1)
      }) should be(List(JustArgs(Const(1))))
  }

  "lift" should "work with object methods with arguments and type arguments" in {
    testReify(implicit collec =>
      lift {
        ObjectExample.argsAndTArgs[Int, Boolean](1, true)
      }) should be(List(ArgsAndTArgs[Int, Boolean](Const(1), Const(true))))
  }

  "lift" should "work with TArgClassExample methods with size" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].size
      }) should be(List(Size[Int](TArgClassExampleCase[Int]())))
  }

  "lift" should "work with TArgClassExample methods with take" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].take(3)
      }) should be(List(Take[Int](TArgClassExampleCase[Int](), 3)))
  }

  "lift" should "work with TArgClassExample methods with val x" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].x
      }) should be(List(X[Int](TArgClassExampleCase[Int]())))
  }

  "lift" should "work with TArgClassExample methods with val y" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].y
      }) should be(List(Y[Int](TArgClassExampleCase[Int]())))
  }

  "lift" should "work with TArgClassExample methods with method type" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Boolean].z[Int]
      }) should be(List(TArgsZ[Boolean, Int](TArgClassExampleCase[Boolean]())))
  }

  "lift" should "work with TArgClassExample methods with app 0 args" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].app1[Int]()
      }) should be(List(AppManyArgs[Int](TArgClassExampleCase[Int])))
  }

  "lift" should "work with TArgClassExample methods with app 1 args" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].app1[Int](1)
      }) should be(List(AppManyArgs[Int](TArgClassExampleCase[Int], 1))) // Const(1) ?
  }

  "lift" should "work with TArgClassExample methods with app many args" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].app1[Int](1, 2)
      }) should be(List(AppManyArgs[Int](TArgClassExampleCase[Int], Const(1), Const(2))))
  }

  "lift" should "work with construction of TArgClassExample" in {
    testReify(implicit collec =>
      lift {
        new ClassExample
      }) should be(List(ClassCons))
  }
}
