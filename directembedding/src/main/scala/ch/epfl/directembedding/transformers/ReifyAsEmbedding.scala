package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ TypeHelper, DirectEmbeddingModule, DirectEmbeddingUtils, MacroModule }

class reifyAs(to: Any) extends scala.annotation.StaticAnnotation
class ignore extends scala.annotation.StaticAnnotation

trait ReifyAsEmbedding extends DirectEmbeddingModule
  with DirectEmbeddingUtils
  with TypeHelper {
  import c.universe._

  object ReifyAsTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = {
      logStarred("Before ReifyAsEmbedding")
      new ReifyAsTransformer().apply(tree)
    }
  }

  final class ReifyAsTransformer extends Transformer {
    def apply(tree: Tree): Tree = transform(tree)

    private def reifyAnnotation(methodSym: Symbol): Option[Annotation] =
      methodSym.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs])

    private def reify(lhs: Tree, tree: Tree, extraTargs: List[Tree], extraArgs: List[Tree]): Tree = {
      reify(body(lhs, tree), typeArgs(lhs) ::: extraTargs, args(lhs, extraArgs))
    }

    private def reify(body: Tree, targs: List[Tree], args: List[Tree]): Tree = {
      (targs, args) match {
        case (Nil, Nil)    => body
        case (targs, Nil)  => q"$body.apply[..$targs]"
        case (Nil, args)   => q"$body.apply(..$args)"
        case (targs, args) => q"$body.apply[..$targs](..$args)"
      }
    }

    private def isNotModule(tree: Tree): Boolean = {
      val expr = tree match {
        // Typed blocks have no symbol
        case Typed(lhs, typ) => lhs
        case _               => tree
      }
      expr.symbol != null && !expr.symbol.isModule
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

    private def invokedSymbol(field: Select): Symbol = {
      field.symbol.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs]).map { _ =>
        field.symbol
      }.getOrElse {
        // Value member compiled to private variable
        field.symbol.owner.info.members.find(equalSymbols(field.symbol)).getOrElse {
          throw new RuntimeException(s"Unable to get value member $field. ${showRaw(field)}")
        }
      }
    }

    private def self(lhs: Tree): Option[Tree] = lhs match {
      case Select(x, _) if isNotModule(x) => Some(x)
      case TypeApply(Select(x, _), _) if isNotModule(x) => Some(x)
      case _ => None
    }

    private def typeArgs(lhs: Tree): List[Tree] = lhs match {
      case Select(x, _)        => x.tpe.typeArgs.map(TypeTree(_))
      case TypeApply(_, targs) => targs
      case _                   => Nil
    }

    private def args(lhs: Tree, rhs: List[Tree]): List[Tree] = {
      (self(lhs).toList ::: rhs).map(transform)
    }

    private def symbol(lhs: Tree): Symbol = lhs match {
      case Select(New(newBody), _)     => newBody.symbol
      case field: Select               => invokedSymbol(field)
      case TypeApply(field: Select, _) => invokedSymbol(field)
      case _                           => lhs.symbol
    }

    private def selfType(tree: Tree): Type = {
      val owner = tree.symbol.owner
      if (owner.isType) owner.asType.toType
      else throw new RuntimeException(s"Unable to find type of $tree")
    }

    private def fallbackAnnotation(treeSymbol: Symbol, tree: Tree): Option[Annotation] =
      for {
        fallbackType <- typeMap.get(selfType(tree).typeSymbol.fullName)
        fallbackSymbol <- {
          log(s"Falling back to $fallbackType")
          findMatchingSymbol(treeSymbol, fallbackType)
        }
        fallbackAnnotation <- {
          log(s"Found matching symbol in overridden type: $fallbackSymbol")
          reifyAnnotation(fallbackSymbol)
        }
      } yield fallbackAnnotation

    /**
     * Get body of class to be reified through [[reifyAs]] annotation.
     *
     * First, performs lookup for the [[reifyAs]] annotation in the invoked
     * symbol and falls back to a lookup in [[typeMap]] for the type of
     * parameter `tree` if no annotation exists.
     *
     * @param lhs Tree that was invoked
     * @param tree Tree to be reified through annotation
     * @return Reified tree if annotation exists, fails compilation otherwise
     */
    private def body(lhs: Tree, tree: Tree): Tree = {
      val treeSymbol = symbol(lhs)
      reifyAnnotation(treeSymbol).orElse {
        log(s"Missing reifyAs annotation for $treeSymbol")
        fallbackAnnotation(treeSymbol, tree)
      }.map {
        _.tree.children.tail.headOption.getOrElse {
          throw new IllegalArgumentException("Missing argument for reifyAs annotation")
        }
      }.getOrElse {
        c.abort(tree.pos, s"$treeSymbol on ${selfType(tree).typeSymbol} is not supported in $dslName")
      }
    }

    private var indent: Int = 0

    override def transform(tree: Tree): Tree = {
      logIndented(s"transform(): $tree", indent)
      logTree(tree, logLevel + 1)
      indent += 2
      val result = tree match {
        // f(a)(b) => f(a, b)
        case a @ Apply(Apply(_, _), _) =>
          logIndented(s"Apply(Apply)", indent)
          transform(uncurry(a, Nil))

        case Apply(lhs, rhs) =>
          logIndented(s"Apply(lhs, rhs)", indent)
          reify(lhs, tree, Nil, rhs)

        case TypeApply(lhs, targs) =>
          logIndented(s"TypeApply", indent)
          reify(lhs, tree, targs, Nil)

        case lhs @ Select(x, _) =>
          logIndented(s"Select()", indent)
          reify(lhs, tree, Nil, Nil)

        case _: Import =>
          tree

        case _ =>
          logIndented(s"other branch:", indent)
          super.transform(tree)
      }
      indent -= 2
      result
    }
  }
}
