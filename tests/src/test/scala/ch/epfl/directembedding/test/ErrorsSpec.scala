package ch.epfl.directembedding.test

import ch.epfl.directembedding.test.example._
import Typecheck._
import org.scalatest.{ FlatSpec, ShouldMatchers }

class ErrorsSpec extends FlatSpec with ShouldMatchers with ExampleTester {

  "dsl" should "give a useful error when a reifyAs annotation is missing" in {
    typedWithMsg(
      """
        dsl {
          ObjectExample.missingAnnotation
        }
      """, "method missingAnnotation on object ObjectExample is not supported in example.dsl")
  }

  "dsl" should "give a useful error when a reifyAs annotation is missing in fallback type" in {
    typedWithMsg(
      """
        dsl {
          val s = "foobar"
          s.charAt(1)
        }
      """, "method charAt on class String is not supported in example.dsl")
  }

}
