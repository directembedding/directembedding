package ch.epfl.directembedding.test

import ch.epfl.directembedding.{ DslConfig, DETransformer }
import ch.epfl.directembedding.transformers.{ DSLVirtualization, reifyAs }
import ch.epfl.yinyang.EmbeddedControls

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object example {
  def dsl[T](block: T): T = macro implementations.liftRep[T]

  implicit def liftConstant[T](x: T): Exp[T] = Const(x)

  def compile[T](ast: Exp[T])(implicit collector: Collector): T = {
    collector.add[T](ast)
    ???
  }

  object ExampleConfig extends ExampleConfig

  /**
   * Configuration module for example DSL
   *
   * Note, your custom DSL is free to extend [[DslConfig]]. We don't extend
   * [[DslConfig]] for this example because then we couldn't accept an implicit
   * parameter in the [[dsl()]] method. We use the implicit parameter for
   * testing purposes.
   */
  trait ExampleConfig extends VirtualizationOverrides {
    def dsl[T](e: Exp[T])(implicit collector: Collector): T = {
      collector.add[T](e)
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
      DETransformer[c.type, T](c)(
        "example.dsl",
        c.weakTypeTag[ExampleConfig].tpe,
        None,
        None)(block)
    }
  }
}
