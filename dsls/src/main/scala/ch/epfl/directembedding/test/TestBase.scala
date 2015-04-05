package ch.epfl.directembedding.test

import ch.epfl.directembedding.transformers.reifyAs
import ch.epfl.directembedding._

// Intermediate Representation
trait Exp[T]
case object ValDef extends Exp[Int]
case object NoArgs extends Exp[Int]
case class JustTargs[T, U]() extends Exp[(T, U)]
case class JustArgs(x: Exp[Int]) extends Exp[Int]
case class ArgsAndTArgs[T, U](t: Exp[T], u: Exp[U]) extends Exp[(T, U)]
case class Const[T](x: T) extends Exp[T]

case class Size[T](self: Exp[T]) extends Exp[Int]
case class Take[T](self: Exp[_], n: Exp[Int]) extends Exp[TArgClassExample[T]]
case class X[T](self: Exp[T]) extends Exp[Int]
case class Y[T](self: Exp[T]) extends Exp[Int]
case class TArgsZ[T, U](self: Exp[T]) extends Exp[U]
case class AppCurry[T](self: Exp[T], p1: Exp[T], p2: Exp[T], p3: Exp[T]*) extends Exp[T]
case class AppManyArgs[T](self: Exp[T], p1: Exp[T]*) extends Exp[T]

case class IF[T](cond: Exp[Boolean], e1: Exp[T], e2: Exp[T]) extends Exp[T]
case class While[T](cond: Exp[Boolean], `do`: Exp[T]) extends Exp[T]
case class DoWhile[T](cond: Exp[Boolean], `do`: Exp[T]) extends Exp[T]
case class Return[T](cond: Exp[T]) extends Exp[T]
case class NewVar[T](e: Exp[T]) extends Exp[T]
case class ReadVar[T](e: Exp[T]) extends Exp[T]
case class Assign[T](x: Exp[T], t: Exp[T]) extends Exp[T]
case class Infix_==[T](t: Exp[T], t1: Exp[T]) extends Exp[Boolean]
case class Infix_Equals[T](t: Exp[T], t1: Exp[T]) extends Exp[Boolean]
case class Infix_Eq[T](t: Exp[T], t1: Exp[T]) extends Exp[Boolean]
case class Infix_Ne[T](t: Exp[T], t1: Exp[T]) extends Exp[Boolean]
case class Infix_!=[T](t: Exp[T], t1: Exp[T]) extends Exp[Boolean]
case class Infix_##[T](t: Exp[T]) extends Exp[Int]
case class Infix_HashCode[T](t: Exp[T]) extends Exp[Int]
case class Infix_IsInstanceOf[T](t: Exp[_]) extends Exp[Boolean]
case class Infix_AsInstanceOf[T](t: Exp[_]) extends Exp[T]
case class Infix_ToString[T](t: Exp[T]) extends Exp[String]
case class Infix_Notify[T](t: Exp[T]) extends Exp[T]
case class Infix_NotifyAll[T](t: Exp[T]) extends Exp[T]
case class Infix_Wait[T](t: Exp[T]) extends Exp[T]
case class Infix_WaitL[T](t: Exp[T], t1: Exp[Long]) extends Exp[T]
case class Infix_WaitTL[T](t: Exp[T], t1: Exp[Long], l: Exp[Int]) extends Exp[T]

// Unable to extract type of self
case class Infix_Synchronized[T](self: Exp[_], t1: Exp[T]) extends Exp[T]
case class Infix_Lamda[T, U](f: T => Exp[U]) extends Exp[T => U]
case class App[T, U](infix_Lamda: Infix_Lamda[T, U]) extends Exp[T => U]
case class Infix_ValDef[T](t: Exp[T]) extends Exp[T]

case object ClassCons extends Exp[ClassExample]
case class TArgClassExampleCase[T]() extends Exp[T]

// Example Object with all corner cases.
object ObjectExample {
  @reifyAs(ValDef)
  val valDef: Int = ???

  def missingAnnotation: Int = ???

  @reifyAs(NoArgs)
  def noArgs: Int = ???

  @reifyAs(JustTargs)
  def justTargs[T, U]: (T, U) = ???

  @reifyAs(JustArgs)
  def justArgs(x: Int): Int = ???

  @reifyAs(ArgsAndTArgs)
  def argsAndTArgs[T, U](t: T, u: U): (T, U) = ???

  object nested {
    @reifyAs(ValDef)
    val valDef: Int = ???

    @reifyAs(NoArgs)
    def noArgs: Int = ???

    @reifyAs(JustTargs)
    def justTargs[T, U]: (T, U) = ???

    @reifyAs(JustArgs)
    def justArgs(x: Int): Int = ???

    @reifyAs(ArgsAndTArgs)
    def argsAndTArgs[T, U](t: T, u: U): (T, U) = ???
  }
}

@reifyAs(TArgClassExampleCase)
class TArgClassExample[T] {
  @reifyAs(Size)
  def size: Int = ???

  @reifyAs(Take)
  def take(n: Int): TArgClassExample[T] = ???

  @reifyAs(X)
  val x: Int = ???

  @reifyAs(Y)
  def y: Int = ???

  @reifyAs(TArgsZ)
  def z[T]: T = ???

  @reifyAs(AppCurry)
  def appCurry1[T](p1: T)(p2: T): T = ???

  @reifyAs(AppCurry)
  def appCurry2[T](p1: T)(p2: T)(p3: T): T = ???

  @reifyAs(AppManyArgs)
  def app1[T](p1: T*): T = ???
}

@reifyAs(ClassCons)
class ClassExample {
  val dummyVal: Int = 1
}
