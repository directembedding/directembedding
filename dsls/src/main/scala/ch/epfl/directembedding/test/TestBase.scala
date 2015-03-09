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

case class Size[T](self: Exp[TArgClassExampleCase[T]]) extends Exp[Int]
case class Take[T](self: Exp[TArgClassExampleCase[T]], n: Exp[Int]) extends Exp[TArgClassExample[T]]
case class X[T](self: Exp[TArgClassExample[T]]) extends Exp[Int]
case class Y[T](self: Exp[TArgClassExample[T]]) extends Exp[Int]
case class TArgsZ[T, U](self: Exp[TArgClassExample[T]]) extends Exp[U]
case class AppCurry[T](self: Exp[TArgClassExample[T]], p1: Exp[T], p2: Exp[T], p3: Exp[T]*) extends Exp[T]
case class AppManyArgs[T](self: Exp[TArgClassExample[T]], p1: Exp[T]*) extends Exp[T]

case object ClassCons extends Exp[ClassExample]
case class TArgClassExampleCase[T]() extends Exp[TArgClassExample[T]]

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

  // TODO nested objects
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
