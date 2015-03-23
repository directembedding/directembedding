package ch.epfl.directembedding.test

import scala.reflect.macros.{ TypecheckException, blackbox }

object Typecheck {
  import scala.language.experimental.macros

  def typed(what: String): Unit = macro TypecheckingMacros.typed
  def typedWithMsg(what: String, msg: String): Unit = macro TypecheckingMacros.typedWithMsg
}

object TypecheckingMacros {

  private class Macro[C <: blackbox.Context](val c: C) {
    def typecheck(what: c.Expr[String],
                  expected: Option[String]): c.Expr[Unit] = {

      import c.universe._

      val Literal(Constant(toCompile)) = what.tree
      try {
        c.typecheck(c.parse(s"{ $toCompile }"))
        c.abort(c.enclosingPosition, "Expected type error, type checked successfully.")
      } catch {
        case e: TypecheckException =>
          val errMsg = e.getMessage
          expected foreach { msg0 =>
            if (errMsg != msg0)
              c.abort(c.enclosingPosition,
                s"Type error messages mismatch.\nExpected: $msg0\nFound: $errMsg")
          }
      }
      c.Expr(q"()")
    }
  }

  def typed(c: blackbox.Context)(what: c.Expr[String]): c.Expr[Unit] = {
    new Macro[c.type](c).typecheck(what, None)
  }

  def typedWithMsg(c: blackbox.Context)(what: c.Expr[String], msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    val Literal(Constant(v0: String)) = msg.tree
    new Macro[c.type](c).typecheck(what, Some(v0))
  }
}
