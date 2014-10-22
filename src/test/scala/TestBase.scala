package ch.epfl.directembedding

// Intermediate Representation
trait Exp[T]
case object ValDef extends Exp[Int]
case object NoArgs extends Exp[Int]
case class JustTArgs[T, U]() extends Exp[(T, U)]
case class JustArgs(x: Exp[Int]) extends Exp[Int]
case class ArgsAndTArgs[T, U](t: Exp[T], u: Exp[U]) extends Exp[(T, U)]
case class Const[T](x: T) extends Exp[T]

// Example Object with all corner cases.
object ObjectExample {
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
  def size: Int = ???
  def take(n: Int): TArgClassExample[T] = ???
  val x: Int = ???
  def y: Int = ???
  def z[T]: T = ???
  def app[T](p1: T)(p2: T): T = ???
  def app1[T](p1: T*): T = ???
}

// TODO
class ClassExample {
}
