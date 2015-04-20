package ch.epfl.directembedding.test

import ch.epfl.directembedding.test.example._
import org.scalatest.{ FlatSpec, ShouldMatchers }

class TypeOverridingSpec extends FlatSpec with ShouldMatchers with ExampleTester {

  "dsl" should "allow custom overriding of Int.+" in {
    runTest(
      dsl {
        val i = 2
        i + 1
      }) should be(IntPlus(Infix_ValDef(2), 1))
  }

  // This case is different from IntPlus because the
  // type of `s` is String and the result is of type Int
  "dsl" should "allow custom overriding of String.length" in {
    runTest(
      dsl {
        val s = "foobar"
        s.length
      }) should be(StringLength(Infix_ValDef("foobar")))
  }

  "dsl" should "allow custom overriding of String.concat" in {
    runTest(
      dsl {
        val s = "foobar"
        s.concat("kaz")
      }) should be(StringConcat(Infix_ValDef("foobar"), "kaz"))
  }

  "dsl" should "allow custom overriding of val.targs[T]" in {
    val l = new ThirdPartyClass()
    runTest(
      dsl {
        l.targs[Int]
      }) should be(MyTArgs[Int](ThirdPartyClass()))
  }

  "dsl" should "allow custom overriding of ThirdPartyClass().targs[T]" in {
    runTest(
      dsl {
        ThirdPartyClass().targs[Int]
      }) should be(MyTArgs[Int](Const(ThirdPartyClass())))
  }

  "dsl" should "allow custom overriding of ThirdPartyObject().targs[T]" in {
    runTest(
      dsl {
        ThirdPartyObject.targs[Int]
      }) should be(MyTArgsObject[Int]())
  }

}
