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
      }) should be(List(ArgsAndTArgs(Const(1), Const(true))))
  }

  "lift" should "work with TArgClassExample methods with size" in {
    testReify(implicit collec =>
      lift {
        new TArgClassExample[Int].size
      }) should be(List(Size))
  }

  "lift" should "work with construction of TArgClassExample" in {
    testReify(implicit collec =>
      lift {
        new ClassExample
      }) should be(List(ClassCons))
  }
}
