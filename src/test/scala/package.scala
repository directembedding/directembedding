package ch.epfl
import scala.language.experimental.macros

package object directembedding {
  def lift[T](block: T): T = macro Macros.lift[T]

  def compile[T](exp: Exp[T]): T = {
    println(exp)
    ???
  }

  implicit def liftConstant[T](x: T): Exp[T] = Const(x)
}
