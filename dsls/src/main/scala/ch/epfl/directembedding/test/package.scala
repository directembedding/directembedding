package ch.epfl.directembedding

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object test {
  // For testing purposes
  def persisted(x: Any): String = macro Persisted.persisted
  def inline[T](x: T): T = macro Persisted.inline[T]
  private object Persisted {
    def persisted(c: Context)(x: c.Expr[Any]): c.Expr[String] = {
      import c.universe._
      val methodSym = x.tree match {
        case Apply(TypeApply(x, targs), args) =>
          x.symbol
        case Apply(x, args) =>
          x.symbol
        case TypeApply(x, targs) =>
          x.symbol
        case field @ Select(x, y) =>
          val symbolAnnotations = field.symbol.annotations.filter(_.tree.tpe <:< c.typeOf[persist])
          if (symbolAnnotations.isEmpty)
            // unfortunately the annotation goes only to the getter
            field.symbol.owner.info.members.filter(x => x.name.toString == field.symbol.name + " ").head
          else field.symbol
      }

      val annotArg = methodSym.annotations.filter(_.tree.tpe <:< c.typeOf[persist]).head.tree.children.tail.head
      val q"""(new ch.epfl.directembedding.MethodTree({
        $tree;
        ()}): ${ _ })""" = annotArg

      c.Expr(q"${showRaw(tree)}")
    }

    def inline[T](c: Context)(x: c.Expr[T]): c.Expr[T] = {
      import c.universe._
      val (methodSym, args) = x.tree match {
        case Apply(TypeApply(x, targs), args) =>
          (x.symbol, args)
        case Apply(x, args) =>
          (x.symbol, args)
        case TypeApply(x, targs) =>
          (x.symbol, Nil)
        case field @ Select(x, y) =>
          val symbolAnnotations = field.symbol.annotations.filter(_.tree.tpe <:< c.typeOf[persist])
          if (symbolAnnotations.isEmpty)
            // unfortunately the annotation goes only to the getter
            (field.symbol.owner.info.members.filter(x => x.name.toString == field.symbol.name + " ").head, Nil)
          else (field.symbol, Nil)
      }

      val annotArg = methodSym.annotations.filter(_.tree.tpe <:< c.typeOf[persist]).head.tree.children.tail.head
      val q"""(new ch.epfl.directembedding.MethodTree({
        $tree;
        ()}): ${ _ })""" = annotArg

      val res = Macros.inlineMethod(c)(tree, args, methodSym.asMethod.paramLists.head)
      c.Expr[T](res)
    }
  }
}
