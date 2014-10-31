package ch.epfl.directembedding.test

import org.scalatest.{ FlatSpec, ShouldMatchers }

class BasicSpec extends FlatSpec with ShouldMatchers {

  //OK
  "lift" should "work object fields" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.valDef
      }
    }
  }

  // "lift" should "work object fields" in {
  //   intercept[scala.NotImplementedError]{
  //     lift {
  //       ObjectExample.valDef
  //     }
  //   }
  //   should be (
  //     """_root_.ch.epfl.directembedding.test.compile(ValDef)"""
  //   )
  // }

  //OK
  it should "work with object methods without arguments and type arguments" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.noArgs
      }
    }
  }

  //OK
  it should "work with object methods with just type arguments" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.justTargs[TArgClassExample[Int], TArgClassExample[Int]]
      }
    }
  }

  //OK
  it should "work with object methods with just arguments" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.justArgs(1)
      }
    }
  }

  //OK
  it should "work with object methods with arguments and type arguments" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.argsAndTArgs[Int, Boolean](1, true)
      }
    }
  }

  //// Nested ////
  //OK
  it should "work with object methods with nested arguments" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.nested.justArgs(1)
      }
    }
  }

  // TArgClassExample
  // it should "work with TArgClassExample methods with size" in {
  //   intercept[scala.NotImplementedError] {
  //     lift {
  //       new TArgClassExample[Int].size
  //     }
  //   }
  // }

}
