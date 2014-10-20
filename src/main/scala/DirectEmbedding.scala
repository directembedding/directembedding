package ch.epfl.directembedding

import scala.reflect.macros.blackbox.Context

class reifyAs(to: Any) extends scala.annotation.StaticAnnotation

protected[directembedding] object Macros {

  def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    /**
     * Transforms methods to their domain-specific IR specified by
     * `reifyAt` annotations.
     */
    class LiftingTransformer extends Transformer {
      def reify(methodSym: Symbol, targs: Option[List[Tree]], args: Option[List[Tree]]): Tree = {
        val reifyAsAnnot = methodSym.annotations.filter(_.tree.tpe <:< c.typeOf[reifyAs]).head
        val body = reifyAsAnnot.scalaArgs.head
        (targs, args) match {
          case (None, None)              => body
          case (Some(targs), None)       => q"${body}.apply[..$targs]"
          case (None, Some(args))        => q"${body}.apply(..$args)"
          case (Some(targs), Some(args)) => q"${body}.apply[..$targs](..$args)"
        }
      }

      override def transform(tree: Tree): Tree = tree match {
        case Apply(TypeApply(x, targs), args) =>
          reify(x.symbol, Some(targs.map(transform(_))), Some(args.map(transform(_))))
        case Apply(x, args) =>
          reify(x.symbol, None, Some(args.map(transform(_))))
        case TypeApply(x, targs) =>
          reify(x.symbol, Some(targs.map(transform(_))), None)
        case field @ Select(x, y) =>
          val symbolAnnotations = field.symbol.annotations.filter(_.tree.tpe <:< c.typeOf[reifyAs])
          val fieldOrGetterSym = if (symbolAnnotations.isEmpty)
            // unfortunately the annotation goes only to the getter
            field.symbol.owner.info.members.filter(x => x.name.toString == field.symbol.name + " ").head
          else field.symbol
          reify(fieldOrGetterSym, None, None)
        case _ =>
          super.transform(tree)
      }
    }

    val reified = new LiftingTransformer().transform(block.tree)
    c.Expr[T](q"_root_.ch.epfl.directembedding.compile($reified)")
  }
}
