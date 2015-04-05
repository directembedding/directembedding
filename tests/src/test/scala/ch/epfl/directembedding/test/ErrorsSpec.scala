package ch.epfl.directembedding.test

import ch.epfl.directembedding.test.example._
import Typecheck._
import org.scalatest.{ FlatSpec, ShouldMatchers }

class ErrorsSpec extends FlatSpec with ShouldMatchers with ExampleTester {

  "dsl" should "give a useful error when a reifyAs annotation is missing" in {
    typedWithMsg(
      """
        runTest(
          dsl {
            ObjectExample.missingAnnotation
        }) should be(???)
      """, "method missingAnnotation is not supported in example.dsl")
  }

}
