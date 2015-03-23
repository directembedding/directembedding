package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils, MacroModule }

class reifyAs(to: Any) extends scala.annotation.StaticAnnotation

trait ReifyAsEmbedding extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._

  override def lift(tree: Tree): Tree = LiftingTransformer(tree)

  object LiftingTransformer {
    def apply(tree: Tree) = {
      new LiftingTransformer(tree.pos).apply(tree)
    }
  }

  final class LiftingTransformer(pos: Position) extends Transformer {
    def apply(tree: Tree): Tree = transform(tree)

    def reify(methodSym: Symbol, targs: List[Tree], args: List[Tree]): Tree = {
      val reifyAsAnnot = methodSym.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs]).getOrElse {
        c.abort(pos, s"Missing reifyAs annotation for $methodSym")
      }

      val body :: _ = reifyAsAnnot.tree.children.tail

      (targs, args) match {
        case (Nil, Nil)    => body
        case (targs, Nil)  => q"${body}.apply[..$targs]"
        case (Nil, args)   => q"${body}.apply(..$args)"
        case (targs, args) => q"${body}.apply[..$targs](..$args)"
      }
    }

    def getInvokedSymbol(field: Select): Symbol = {
      field.symbol.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs]).map { _ =>
        field.symbol
      }.getOrElse {
        // Value member compiled to private variable
        field.symbol.owner.info.members.find(x => x.name.toString.trim == field.symbol.name.toString.trim).getOrElse {
          throw new RuntimeException(s"Unable to get value member $field. ${showRaw(field)}")
        }
      }
    }

    /**
     * Returns transformed tree if tree is a class instance, None otherwise
     */
    def getSelf(x: Tree): Option[Tree] =
      if (x.symbol.isModule) None else Some(transform(x))

    /**
     * Convert curried function to uncurried function, transforming each argument along the way
     *
     * Example f(1)(2) => f(Const(1), Const(2))
     *
     * @param t Tree to be uncurried
     * @param args List[Tree] arguments to prepend to uncurried argument list
     * @return Uncurried tree
     */
    def uncurry(t: Tree, args: List[Tree]): Tree = t match {
      case field @ Apply(x, y) =>
        uncurry(x, y ::: args)
      case _ =>
        Apply(t, args.map(transform(_)))
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case a @ Apply(Apply(_, _), _) =>
          val t = uncurry(a, Nil)
          transform(t)

        case Apply(Select(New(newBody), _), _) =>
          val listArgs = newBody.tpe.typeArgs
          reify(newBody.symbol, listArgs.map(TypeTree(_)), Nil)

        case Apply(field @ Select(x, _), args) =>
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, x.tpe.typeArgs.map(TypeTree(_)), self ::: args)

        case Apply(TypeApply(field @ Select(x, _), targs), args) =>
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, targs.map(transform(_)), self ::: args.map(transform(_)))

        case TypeApply(field @ Select(x, _), targs) =>
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, x.tpe.typeArgs.map(TypeTree(_)) ::: targs.map(transform(_)), self)

        case field @ Select(x, _) =>
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, x.tpe.typeArgs.map(TypeTree(_)), self)

        case Block(_, expr) => transform(expr)

        case _              => super.transform(tree)
      }
    }
  }
}
