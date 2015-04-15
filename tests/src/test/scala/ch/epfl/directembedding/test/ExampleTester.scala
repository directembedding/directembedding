package ch.epfl.directembedding.test

import org.scalatest.{ FlatSpec, ShouldMatchers }
import ch.epfl.directembedding.test.example._

trait ExampleTester extends FlatSpec {
  def runTest[T](body: => T): Exp[T] = {
    intercept[scala.NotImplementedError] {
      body
    }
    Q.poll().asInstanceOf[Exp[T]]
  }

}
