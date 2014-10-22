package ch.epfl.directembedding

import scala.reflect.macros.blackbox.Context

class reifyAs(to: Any) extends scala.annotation.StaticAnnotation
final class MethodTree(x: Any)

class persist(id: MethodTree) extends scala.annotation.StaticAnnotation

protected[directembedding] object Macros {

  def inlineMethod(c: Context)(f: c.Tree, args: List[c.Tree], params: List[c.Symbol]): c.Tree = {
    import c.universe._
    import internal._, decorators._
    val q"def ${ _ }(..$params2): $tpe = $body" = f
    val paramsMap = (params zip args).map {
      case (param, arg) =>
        val temp = c.freshName(TermName(param.name.toString))
        val tempSym = c.internal.enclosingOwner.newTermSymbol(temp).setInfo(arg.tpe.widen)
        val valDef = c.internal.valDef(tempSym, c.internal.changeOwner(arg, c.internal.enclosingOwner, tempSym))

        (param.symbol, (tempSym, valDef))
    }.toMap

    // put a name of the val
    val inlinedBody = c.internal.typingTransform(body)((tree, api) => tree match {
      case i @ Ident(_) if paramsMap contains tree.symbol =>
        val sym = paramsMap(tree.symbol)._1
        api.typecheck(q"$sym")
      case _ =>
        api.default(tree)
    })

    q"""{
      ..${paramsMap.values.map(_._2)}
      ${c.untypecheck(inlinedBody)}
    }"""
  }

  def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    /**
     * Transforms methods to their domain-specific IR specified by
     * `reifyAt` annotations.
     */
    class LiftingTransformer extends Transformer {
      def reify(methodSym: Symbol, targs: List[Tree], args: List[Tree]): Tree = {
        val reifyAsAnnot = methodSym.annotations.filter(_.tree.tpe <:< c.typeOf[reifyAs]).head
        val body = reifyAsAnnot.tree.children.tail.head

        (targs, args) match {
          case (Nil, Nil)    => body
          case (targs, Nil)  => q"${body}.apply[..$targs]"
          case (Nil, args)   => q"${body}.apply(..$args)"
          case (targs, args) => q"${body}.apply[..$targs](..$args)"
        }
      }

      def getAnnot(field: Select): Symbol = {
        val symbolAnnotations = field.symbol.annotations.filter(_.tree.tpe <:< c.typeOf[reifyAs])
        val fieldOrGetterSym = if (symbolAnnotations.isEmpty)
          // unfortunately the annotation goes only to the getter
          field.symbol.owner.info.members.filter(x => x.name.toString == field.symbol.name + " ").head
        else field.symbol

        fieldOrGetterSym
      }

      def getSelf(x: Tree): List[Tree] = x match {
        case Select(_, _) => Nil
        case _ =>
          if (x.symbol.isModule) {
            Nil

          } else {
            List(transform(x))
          }
      }

      def traverserHelper(t: Tree, args: List[Tree]): Tree = t match {
        case field @ Apply(x, y) =>
          traverserHelper(x, y ::: args)
        case _ =>
          Apply(t, args.map(transform(_)))
      }

      override def transform(tree: Tree): Tree = {
        tree match {
          case a @ Apply(Apply(x, y2), y1) =>
            val t = traverserHelper(a, Nil)
            transform(t)

          case Apply(Select(New(newBody), y), args) =>
            val listArgs = newBody.tpe.typeArgs
            reify(newBody.symbol, listArgs.map(TypeTree(_)), Nil)

          case Apply(field @ Select(x, y), args) =>
            val fieldOrGetterSym = getAnnot(field)
            val self = getSelf(x)
            reify(fieldOrGetterSym, x.tpe.typeArgs.map(TypeTree(_)), self ::: args)

          case Apply(TypeApply(field @ Select(x, y), targs), args) =>
            val fieldOrGetterSym = getAnnot(field)
            val self = getSelf(x)
            reify(fieldOrGetterSym, targs.map(transform(_)), self ::: args.map(transform(_)))

          case TypeApply(field @ Select(x, y), targs) =>
            val fieldOrGetterSym = getAnnot(field)
            val self = getSelf(x)
            reify(fieldOrGetterSym, x.tpe.typeArgs.map(TypeTree(_)) ::: targs.map(transform(_)), self)

          case field @ Select(x, y) =>
            val fieldOrGetterSym = getAnnot(field)
            val self = getSelf(x)
            reify(fieldOrGetterSym, x.tpe.typeArgs.map(TypeTree(_)), self)

          case x =>
            super.transform(tree)
        }
      }
    }

    val reified = new LiftingTransformer().transform(block.tree)
    c.Expr[T](q"_root_.ch.epfl.directembedding.test.compile($reified)")
  }

  def extractMethodTree(c: Context)(x: c.Expr[Any]): c.Expr[MethodTree] = {
    import c.universe._
    val argPos = x.tree.pos
    def isPersist(annot: Tree): Boolean = annot match {
      case Apply(Select(New(tn), termNames.CONSTRUCTOR), q"()" :: Nil) =>
        Set(
          "persist",
          "_root_.ch.epfl.directembedding.persist",
          "ch.epfl.directembedding.persist",
          "epfl.directembedding.persist",
          "directembedding.persist") contains tn.toString
      case _ =>
        false
    }

    def findAnnot(mods: Modifiers): Option[Tree] = mods.annotations.filter(isPersist) find {
      case Apply(_, args) if argPos == args.head.pos => true
      case _                                         => false
    }

    val defTreeList = (c.enclosingUnit.body: @unchecked).collect {
      case defdef @ DefDef(mods, p1, p2, p3, p4, p5) if findAnnot(mods).isDefined =>
        DefDef(
          Modifiers.apply(mods.flags, mods.privateWithin, mods.annotations.filterNot(isPersist)),
          p1, p2, p3, p4, p5)
    }

    if (defTreeList.isEmpty)
      c.abort(x.tree.pos,
        "@persist not found. Potential reason for this error is if @persist is generated by a meta-program.")

    val defTree = defTreeList.head
    c.Expr(q"new MethodTree({$defTree})")
  }

}
