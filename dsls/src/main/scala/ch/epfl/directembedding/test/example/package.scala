package ch.epfl.directembedding.test

import ch.epfl.directembedding.{ DslConfig, DETransformer }
import ch.epfl.directembedding.transformers.{ DSLVirtualization, reifyAs }
import ch.epfl.yinyang.EmbeddedControls

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object example {
  def dsl[T](block: T): T = macro implementations.liftRep[T]

  val Q = new java.util.concurrent.LinkedBlockingQueue[Exp[_]]()

  implicit def liftConstant[T](x: T): Exp[T] = Const(x)

  object ExampleConfig extends ExampleConfig

  /**
   * Configuration module for example DSL
   */
  trait ExampleConfig extends VirtualizationOverrides with DslConfig {

    type Literal[T] = Const[T]
    type Rep[T] = Exp[T]

    def dsl[T](e: Exp[T]): T = {
      Q.add(e)
      ???
    }

    def lift[T](e: T): Const[T] = Const(e)
  }

  trait VirtualizationOverrides {
    @reifyAs(IF)
    def __ifThenElse[T](cond: Boolean, e1: T, e2: T): T = ???

    @reifyAs(NewVar)
    def __newVar(v: String): String = ???

  }

  object implementations {
    def liftRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      val config = "ch.epfl.directembedding.test.example"
      DETransformer[c.type, T, ExampleConfig](c)(
        "example.dsl",
        None,
        // Note the explicit `apply`, this is necessary.
        None).apply(block)
    }
  }
}
