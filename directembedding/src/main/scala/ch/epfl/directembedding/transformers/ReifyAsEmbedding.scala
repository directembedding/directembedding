package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils, MacroModule }

class reifyAs(to: Any) extends scala.annotation.StaticAnnotation
class ignore extends scala.annotation.StaticAnnotation

trait ReifyAsEmbedding extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._

  object ReifyAsTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = {
      logStarred("Before ReifyAsEmbedding", logLevel)
      new ReifyAsTransformer(tree.pos).apply(tree)
    }
  }

  final class ReifyAsTransformer(pos: Position) extends Transformer {
    def apply(tree: Tree): Tree = transform(tree)

    private def getReifyAnnotation(methodSym: Symbol): Option[Annotation] =
      methodSym.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs])

    private def reify(methodSym: Symbol, targs: List[Tree], args: List[Tree]): Tree = {
      val annotation = getReifyAnnotation(methodSym).getOrElse {
        c.abort(pos, s"$methodSym is not supported in $dslName")
      }
      reify(annotation.tree.children.tail.head, targs, args)
    }

    private def reify(body: Tree, targs: List[Tree], args: List[Tree]): Tree = {
      (targs, args) match {
        case (Nil, Nil)    => body
        case (targs, Nil)  => q"$body.apply[..$targs]"
        case (Nil, args)   => q"$body.apply(..$args)"
        case (targs, args) => q"$body.apply[..$targs](..$args)"
      }
    }

    private def getInvokedSymbol(field: Select): Symbol = {
      field.symbol.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs]).map { _ =>
        field.symbol
      }.getOrElse {
        // Value member compiled to private variable
        field.symbol.owner.info.members.find(x => x.name.toString.trim == field.symbol.name.toString.trim).getOrElse {
          throw new RuntimeException(s"Unable to get value member $field. ${showRaw(field)}")
        }
      }
    }

    private def isNotModule(tree: Tree): Boolean =
      tree.symbol != null && !tree.symbol.isModule

    /**
     * Returns transformed tree if tree is a class instance, None otherwise
     */
    private def getSelf(lhs: Tree): Option[Tree] = lhs match {
      case Select(x, _) if isNotModule(x) => Some(x)
      case TypeApply(Select(x, _), _) if isNotModule(x) => Some(x)
      case _ => None
    }

    /**
     * Convert curried function to uncurried function
     *
     * Example f(1)(2) => f(1, 2)
     *
     * @param t Tree to be uncurried
     * @param args List[Tree] arguments to prepend to uncurried argument list
     * @return Uncurried tree
     */
    private def uncurry(t: Tree, args: List[Tree]): Tree = t match {
      case field @ Apply(x, y) =>
        uncurry(x, y ::: args)
      case _ =>
        Apply(t, args)
    }

    private var indent: Int = 0

    private def typeArgs(lhs: Tree): List[Tree] = lhs match {
      case Select(x, _)        => x.tpe.typeArgs.map(TypeTree(_))
      case TypeApply(_, targs) => targs
      case _                   => Nil
    }

    private def args(lhs: Tree, rhs: List[Tree]): List[Tree] =
      (getSelf(lhs).toList ::: rhs).map(transform)

    private def symbol(lhs: Tree): Symbol = lhs match {
      case Select(New(newBody), _)            => newBody.symbol
      case field @ Select(_, _)               => getInvokedSymbol(field)
      case TypeApply(field @ Select(_, _), _) => getInvokedSymbol(field)
      case _                                  => lhs.symbol
    }

    override def transform(tree: Tree): Tree = {
      logIndented(s"transform(): $tree", indent, logLevel)
      logTree(tree, logLevel + 1)
      indent += 2
      val result = tree match {
        case a @ Apply(Apply(_, _), _) =>
          logIndented(s"Apply(Apply)", indent, logLevel)
          transform(uncurry(a, Nil))

        case Apply(lhs, rhs) =>
          logIndented(s"Apply(lhs, rhs)", indent, logLevel)
          reify(symbol(lhs), typeArgs(lhs), args(lhs, rhs))

        case TypeApply(lhs, targs) =>
          logIndented(s"TypeApply", indent, logLevel)
          reify(symbol(lhs), typeArgs(lhs) ::: targs, args(lhs, Nil))

        case lhs @ Select(x, _) =>
          logIndented(s"Select()", indent, logLevel)
          reify(symbol(lhs), typeArgs(lhs), args(lhs, Nil))

        case _: Import =>
          tree

        case _ =>
          logIndented(s"other branch:", indent, logLevel)
          super.transform(tree)
      }
      indent -= 2
      result
    }
  }
}
