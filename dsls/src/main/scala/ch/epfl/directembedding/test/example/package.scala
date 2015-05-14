package ch.epfl.directembedding.test

import ch.epfl.directembedding.{ MacroModule, DslConfig, DETransformer }
import ch.epfl.directembedding.transformers.{ DSLVirtualization, reifyAs }
import ch.epfl.yinyang.EmbeddedControls

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object example {
  def dsl[T](block: T): T = macro implementations.liftRepNoDebug[T]

  def dslDebug[T](block: T): T = macro implementations.liftRepDebug[T]

  val Q = new java.util.concurrent.LinkedBlockingQueue[Exp[_]]()

  implicit def liftConstant[T](x: T): Exp[T] = Const(x)

  object ExampleConfig extends ExampleConfig

  /**
   * Configuration module for example DSL
   */
  trait ExampleConfig extends DslConfig
    with VirtualizationOverrides //    with ExampleTypeOverrides
    {

    type Literal[T] = Const[T]
    type Rep[T] = Exp[T]

    def compile[T](e: Exp[T]): T = {
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

    @reifyAs(ReadVar)
    def __readVar(v: String): String = ???

    @reifyAs(Assign)
    def __assign(x: String, t: String): String = ???

    @reifyAs(While)
    def __whileDo(cond: Boolean, t: String): String = ???

    @reifyAs(DoWhile)
    def __doWhile(cond: Boolean, t: String): String = ???

    @reifyAs(Return)
    def __return(ret: String): String = ???

    @reifyAs(Infix_==)
    def infix_==(t: String, t1: String): String = ???

    @reifyAs(Infix_Eq)
    def infix_eq(t: String, t1: String): String = ???

    @reifyAs(Infix_Ne)
    def infix_ne(t: String, t1: String): String = ???

    @reifyAs(Infix_!=)
    def infix_!=(t: String, t1: String): String = ???

    @reifyAs(Infix_##)
    def infix_##(t: String): String = ???

    @reifyAs(Infix_Equals)
    def infix_equals(t: String, t1: String): String = ???

    @reifyAs(Infix_HashCode)
    def infix_hashCode(t: String): String = ???

    @reifyAs(Infix_IsInstanceOf)
    def infix_isInstanceOf[T](t: String): T = ???

    @reifyAs(Infix_AsInstanceOf)
    def infix_asInstanceOf[T](t: String): T = ???

    @reifyAs(Infix_ToString)
    def infix_toString(t: Int): String = ???

    @reifyAs(Infix_Notify)
    def infix_notify(t: String): String = ???

    @reifyAs(Infix_NotifyAll)
    def infix_notifyAll(t: String): String = ???

    @reifyAs(Infix_Wait)
    def infix_wait(t: String): String = ???

    @reifyAs(Infix_WaitL)
    def infix_wait(t: String, t1: Long): String = ???

    @reifyAs(Infix_WaitTL)
    def infix_wait(t: String, t1: Long, l: Int): String = ???

    @reifyAs(Infix_Synchronized)
    def infix_synchronized[T](t: String, t1: Int): T = ???

    @reifyAs(Infix_Lamda)
    def __lambda(f: String => Int): String => Int = ???

    @reifyAs(App)
    def __app(v: String => Int): String => Int = ???

    @reifyAs(App)
    def apply(v: String => Int): String => Int = ???

    @reifyAs(Infix_ValDef)
    def __valDef(v: String): String = ???

    @reifyAs(Infix_ValDef)
    def __valDef(v: Int): Int = ???
  }

  object Config extends ExampleConfig

  object implementations {
    def liftRepDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(true)(c)(block)

    def liftRepNoDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      liftRep(false)(c)(block)

    def liftRep[T](debug: Boolean)(c: Context)(block: c.Expr[T]): c.Expr[T] = {
      import c.universe._
      val config = "ch.epfl.directembedding.test.example"
      DETransformer[c.type, T, ExampleConfig](c)(
        "example.dsl",
        Config,
        Map(
          c.typeTag[Int].tpe -> c.typeTag[MyInt].tpe,
          c.typeTag[String].tpe -> c.typeTag[MyString].tpe,
          c.typeTag[ThirdPartyClass].tpe -> c.typeTag[MyThirdPartyLibrary].tpe,
          c.typeTag[ThirdPartyObject].tpe -> c.typeTag[MyThirdPartyObject].tpe),
        Map.empty,
        Set.empty,
        None,
        // Note the explicit `apply`, this is necessary.
        None,
        if (debug) 2 else 0).apply(block)
    }
  }
}
