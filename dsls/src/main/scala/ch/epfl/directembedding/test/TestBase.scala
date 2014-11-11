package ch.epfl.directembedding.test

import ch.epfl.directembedding._

// Intermediate Representation
trait Exp[T]
case object ValDef extends Exp[Int]
case object NoArgs extends Exp[Int]
case class JustTArgs[T, U]() extends Exp[(T, U)]
case class JustArgs(x: Exp[Int]) extends Exp[Int]
case class ArgsAndTArgs[T, U](t: Exp[T], u: Exp[U]) extends Exp[(T, U)]
case class Const[T](x: T) extends Exp[T]
case object ClassCons

case object Size extends Exp[Int]
case object Take extends Exp[Int]
case object X extends Exp[Int]
case object Y extends Exp[Int]
case class TArgsZ[T]() extends Exp[T]
case class AppCurry[T](p1: Exp[T])(p2: Exp[T]) extends Exp[T]
case class AppManyArgs[T](p1: Exp[T]*) extends Exp[T]

// Example Object with all corner cases.
object ObjectExample {
  @reifyAs(ValDef)
  val valDef: Int = ???

  @reifyAs(NoArgs)
  def noArgs: Int = ???

  @reifyAs(JustTArgs)
  def justTArgs[T, U]: (T, U) = ???

  @reifyAs(JustArgs)
  def justArgs(x: Int): Int = ???

  @reifyAs(ArgsAndTArgs)
  def argsAndTArgs[T, U](t: T, u: U): (T, U) = ???

  // TODO nested objects
  object nested {
    @reifyAs(ValDef)
    val valDef: Int = ???

    @reifyAs(NoArgs)
    def noArgs: Int = ???

    @reifyAs(JustTArgs)
    def justTargs[T, U]: (T, U) = ???

    @reifyAs(JustArgs)
    def justArgs(x: Int): Int = ???

    @reifyAs(ArgsAndTArgs)
    def argsAndTArgs[T, U](t: T, u: U): (T, U) = ???

  }
}

// TODO
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
  def app[T](p1: T)(p2: T): T = ???

  @reifyAs(AppManyArgs)
  def app1[T](p1: T*): T = ???
}

// TODO
@reifyAs(ClassCons)
class ClassExample {
  val valX = 1
}

