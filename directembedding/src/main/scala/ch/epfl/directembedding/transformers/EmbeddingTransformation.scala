package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ TypeHelper, DirectEmbeddingModule, DirectEmbeddingUtils, MacroModule }

class DirectEmbeddingAnnotation extends scala.annotation.StaticAnnotation
class reifyAs(to: Any) extends DirectEmbeddingAnnotation

/**
 * Reify on the lifted "self" object with the same method name.
 *
 * Example:
 *
 *   direct.Query.take(1) => lift(direct.Query).take(lift(1))
 *
 * where the `take` in direct.Query has @reifyAsInvoked.
 *
 * An experimental reification that may be useful for existing
 * deep DSLs, such as Slick. Instead of reifiying into a custom
 * method (endpoint), the existing underlying API on the deep DSL
 * is invoked directly.
 */
class reifyAsInvoked extends DirectEmbeddingAnnotation

/**
 * Same as [[reifyAsInvoked]] except it can be annotated on modules.
 *
 * @param obj The object that the method is invoked on, "self".
 */
class reifyAsInvokedFrom(obj: Any) extends DirectEmbeddingAnnotation

/**
 * No reificaation, the tree is passed through.
 *
 * This can be useful if you generate trees in the preprocessing
 * step (e.g., type providers) that are already part of the deep DSL.
 */
class passThrough extends scala.annotation.StaticAnnotation

trait EmbeddingTransformation extends DirectEmbeddingModule
  with DirectEmbeddingUtils
  with ReificationTransformation
  with TypeHelper {
  import c.universe._

  object EmbeddingTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = {
      logStarred("Before ReifyAsEmbedding")
      new EmbeddingTransformer().apply(tree)
    }
  }

  final class EmbeddingTransformer extends Transformer {
    def apply(tree: Tree): Tree = transform(tree)

    private def reifyAnnotation(methodSym: Symbol): Option[Annotation] =
      methodSym.annotations.find(_.tree.tpe =:= c.typeOf[reifyAs])

    private def annotations(sym: Symbol, tree: Tree): List[Annotation] = {
      fallbackAnnotation(sym, tree).toList ::: sym.annotations
    }

    private def reify(lhs: Tree, tree: Tree, extraTargs: List[Tree], extraArgs: List[Tree]): Tree = {
      val sym = symbol(lhs)
      val reification: List[Reification] = annotations(sym, tree).collect {
        case a if a.tree.tpe =:= c.typeOf[reifyAs] =>
          ReifyAsReification(
            annotationBody(a),
            hiddenTypeArgs(lhs) ::: extraTargs,
            (self(lhs).toList ::: extraArgs).map(transform))
        case a if a.tree.tpe =:= c.typeOf[reifyAsInvoked] =>
          val s = self(lhs).getOrElse {
            c.abort(tree.pos, "reifyAsInvoked cannot be annotated to an object method")
          }
          ReifyAsInvokedReification(transform(s), sym.name.toTermName, extraArgs.map(transform))
        case a if a.tree.tpe =:= c.typeOf[reifyAsInvokedFrom] =>
          val s = a.tree.children.tail.headOption.getOrElse {
            c.abort(tree.pos, "Missing argument for @reifyAsInvokedFrom annotation")
          }
          ReifyAsInvokedReification(s, sym.name.toTermName, extraArgs.map(transform))
        case a if a.tree.tpe =:= c.typeOf[passThrough] =>
          PassThroughReification(tree)
      }

      reification match {
        case Nil =>
          c.abort(tree.pos, s"$sym on ${selfType(tree).typeSymbol} is not supported in $dslName")
        case r :: Nil =>
          ReifyTransform(r)
        case _ =>
          c.abort(tree.pos, s"Ambiguous reification with alteratives:\n${reification.mkString("  ", "\n", "")}")
      }
    }

    private def isNotModule(tree: Tree): Boolean = {
      val expr = tree match {
        // Typed blocks have no symbol
        case Typed(lhs, typ) => lhs
        case Block(_, rhs)   => rhs
        case _               => tree
      }
      expr.symbol != null && !expr.symbol.isModule
    }

    /**
     * Convert curried function to uncurried function.
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

    /**
     * Get correct symbol of invoked member.
     *
     * Handles the case when the member is a val, which scalac translates
     * into a private member.
     * @param field
     * @return
     */
    private def invokedSymbol(field: Select): Symbol = {
      val reifies = field.symbol.annotations.find(_.tree.tpe <:< c.typeOf[DirectEmbeddingAnnotation])
      reifies.map(_ => field.symbol).getOrElse { // find annotations on desugared vals
        field.symbol.owner.info.members.find(equalSymbols(field.symbol)).getOrElse {
          c.abort(field.pos, s"Unable to find symbol for val $field.")
        }
      }
    }

    /**
     * Get body of invoked instance.
     *
     * Example: {{{
     *   self(q"Predef.implicitly[T]") == None
     *   self(q"new Foo()") == Some(q"Foo")
     * }}}
     */
    private def self(lhs: Tree): Option[Tree] = lhs match {
      case Select(x, _) if isNotModule(x) => Some(x)
      case TypeApply(Select(x, _), _) if isNotModule(x) => Some(x)
      case _ => None
    }

    /**
     * Get types that are embedded inside the left-hand side of the application.
     *
     * Example: {{{
     *   hiddenTypeArgs(q"new TArgClassExample[Int]") == List(tq"Int")
     * }}}
     */
    private def hiddenTypeArgs(lhs: Tree): List[Tree] = lhs match {
      case Select(x, _)        => x.tpe.typeArgs.map(TypeTree(_))
      case TypeApply(_, targs) => targs
      case _                   => Nil
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
      else c.abort(tree.pos, s"Unable to resolve type for $tree")
    }

    /**
     * Used for finding annotations for predefined/third-party definitions by looking
     * up into the type map that points to the fallback type.
     */
    private def fallbackAnnotation(treeSymbol: Symbol, tree: Tree): Iterable[Annotation] = {
      val sym = for {
        fallbackType <- typeMap.get(selfType(tree).typeSymbol.fullName)
        fallbackSymbol <- {
          log(s"Falling back to $fallbackType")
          findMatchingSymbol(treeSymbol, fallbackType)
        }
      } yield fallbackSymbol
      sym.toList.flatMap(_.annotations)
    }

    /**
     * Strip away sugar from partially applied functions in annotations.
     *
     * Example:
     *   {{{@reifyAs(method _)}}}
     * is eta expanded into:
     *   {{{(args: Args*) => method(args)}}}
     * {{{
     *   stripEtaExpansion(q"method ") == q"method"
     * }}}
     */
    private def stripEtaExpansion(tree: Tree): Option[Tree] = {
      tree match {
        case Block(_, Function(lhs, rhs)) => rhs.collect {
          case Apply(s: Select, _)     => s
          case TypeApply(s: Select, _) => s
        }.headOption
        case _ => Some(tree)
      }
    }

    private def annotationBody(annotation: Annotation): Tree =
      annotation.tree.children.tail.headOption.flatMap(stripEtaExpansion).getOrElse {
        c.abort(annotation.tree.pos, s"Missing argument for $annotation")
      }

    private var indent: Int = 0

    override def transform(tree: Tree): Tree = {
      logIndented(s"transform(): $tree", indent)
      logTree(tree, logLevel + 1)
      indent += 2
      val result = tree match {
        // f(a)(b) => f(a, b)
        case a @ Apply(Apply(_, _), _) if flattenCurriedFunctions =>
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
          logIndented("Ignored Import", indent)
          tree

        case _: ClassDef =>
          logIndented("Ignored ClassDef", indent)
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
