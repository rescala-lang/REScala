package loci
package transmitter

import scala.collection.mutable
import scala.concurrent.Future
import scala.reflect.macros.whitebox

object TransmittableResolution {
  def apply[
      B: c.WeakTypeTag,
      I: c.WeakTypeTag,
      R: c.WeakTypeTag,
      P: c.WeakTypeTag,
      T: c.WeakTypeTag](c: whitebox.Context)(dummy: c.Tree): c.Tree = {
    import c.universe._

    def dealiasExistentials(tpe: Type) = tpe map {
      case tpe @ ExistentialType(quantified, underlying) =>
        val dealised = underlying map {
          case tpe if tpe exists { tpe => quantified contains tpe.typeSymbol } => tpe.dealias
          case tpe => tpe
        }

        if (!(dealised exists { tpe => quantified contains tpe.typeSymbol }))
          dealised
        else
          tpe

      case tpe =>
        tpe
    }

    val B = dealiasExistentials(weakTypeOf[B])
    val I = dealiasExistentials(weakTypeOf[I])
    val R = dealiasExistentials(weakTypeOf[R])
    val P = dealiasExistentials(weakTypeOf[P])
    val T = dealiasExistentials(weakTypeOf[T])

    val resolutionModule = symbolOf[TransmittableBase.Resolution.type]
    val nothing = B =:= definitions.NothingTpe || B.typeSymbol.owner.owner == resolutionModule

    if (!nothing) {
      val wrapperAlternationClass = symbolOf[TransmittableBase.WrapperAlternation]
      val wrapperModule = symbolOf[TransmittableBase.Wrapper.type]

      val transmittableParameters = symbolOf[Transmittable[_, _, _]].typeParams
      val wrapperParameters = symbolOf[TransmittableBase.Wrapper[_, _, _, _, _]].typeParams
      val pseudoContravariantParameters = List(transmittableParameters.head, wrapperParameters.head)
      val pseudoCovariantParameters = List(transmittableParameters(2), wrapperParameters(2))

      val identicallyTransmittableType = typeOf[IdenticallyTransmittable[_]]
      val identicallyTransmittableTree = q"${termNames.ROOTPKG}.loci.transmitter.IdenticallyTransmittable"
      val wrapperTree = tq"${termNames.ROOTPKG}.loci.transmitter.TransmittableBase.Wrapper"

      // construct `TransmittableBase.DependantValue[B, I, R, Transmittable.Aux[B, I, R, P, T]]` type for implicit resolution
      // replace type parameters of macro application that are not inferred with existentials
      // replace type parameters and `Nothing` types with different `Transmittable.SurrogateType` types
      // remembering the corresponding original type
      val (resolutionType, surrogates) = {
        val TypeRef(surrogateTypePre, surrogateTypeSym, _) =
          typeOf[TransmittableBase.SurrogateType[Any, Any, Any]]: @unchecked
        val TypeRef(dependantPre, dependantSym, _) =
          typeOf[TransmittableBase.DependantValue[Any, Any, Any, Any]]: @unchecked
        val ExistentialType(existentialQuantified, TypeRef(auxPre, auxSym, existentialArgs)) =
          typeOf[Transmittable.Aux[_, _, _, _, _]]: @unchecked

        var count = 0
        var surrogates = List.empty[(Type, Type)]
        var quantified = List.empty[Symbol]
        var args = List.empty[Type]

        def createArg(tpe: Type, index: Int) =
          if (tpe.typeSymbol.owner.owner == resolutionModule) {
            quantified ::= existentialQuantified(index)
            args ::= existentialArgs(index)
          }
          else
            args ::= tpe map {
              case tpe if tpe.typeSymbol.isParameter || tpe =:= definitions.NothingTpe =>
                (surrogates
                  collectFirst { case (surrogate, original) if original =:= tpe => surrogate }
                  getOrElse {
                    val surrogate = internal.typeRef(surrogateTypePre, surrogateTypeSym,
                      List(internal.constantType(Constant(count)), tpe, internal.constantType(Constant(tpe.toString))))
                    count += 1

                    surrogates ::= surrogate -> tpe
                    surrogate
                  })

              case tpe =>
                tpe
            }

        List(T, P, R, I, B).zipWithIndex foreach (createArg _).tupled

        val typeRef = internal.typeRef(dependantPre, dependantSym,
          (args take 3) :+ internal.typeRef(auxPre, auxSym, args))

        if (quantified.isEmpty)
          typeRef -> surrogates
        else
          internal.existentialType(quantified, typeRef) -> surrogates
      }


      // resolve `TransmittableBase.DependantValue[B, I, R, Transmittable.Aux[B, I, R, P, T]]` value
      // extract `TransmittableBase.Any` instance
      val variantParameters = pseudoContravariantParameters.head.asType.isContravariant
      if (!variantParameters) {
        pseudoContravariantParameters foreach { c.internal.setFlag(_, Flag.CONTRAVARIANT) }
        pseudoCovariantParameters foreach { c.internal.setFlag(_, Flag.COVARIANT) }
      }

      val resolutionTree = c inferImplicitValue resolutionType match {
        case q"$_[..$_]($resolution[..$_]($expr))"
          if resolution.symbol.owner == wrapperModule ||
             resolution.symbol.owner == wrapperAlternationClass =>
          expr
        case q"$_[..$_]($expr)" =>
          expr
        case _ =>
          EmptyTree
      }

      if (!variantParameters) {
        pseudoContravariantParameters foreach { c.internal.resetFlag(_, Flag.CONTRAVARIANT) }
        pseudoCovariantParameters foreach { c.internal.resetFlag(_, Flag.COVARIANT) }
      }

      if (resolutionTree.isEmpty)
        c.abort(c.enclosingPosition,
          s"Could not resolve ${resolutionType.typeConstructor} for $B")


      def dealiasNonRepresentableType(tpe: Type): Type =
        tpe map {
          case tpe @ TypeRef(pre @ ThisType(_), sym, _)
            if sym.asType.isAliasType &&
               pre.typeSymbol.name.toString == "<refinement>" =>
            val dealiased = sym.info.asSeenFrom(pre, sym.owner)
            if (dealiased ne tpe)
              dealiasNonRepresentableType(dealiased)
            else
              tpe

          case tpe =>
            tpe
        }

      def hasNonRepresentableType(tpe: Type): Boolean = {
        val symbols = mutable.Set.empty[Symbol]

        tpe foreach {
          case ExistentialType(quantified, _) =>
            symbols ++= quantified
          case _ =>
        }

        tpe exists {
          case tpe @ ThisType(_) =>
            tpe.typeSymbol.name.toString == "<refinement>"
          case tpe =>
            !(symbols contains tpe.typeSymbol) && (tpe.typeSymbol.name.toString endsWith ".type")
        }
      }

      // restore original types for types replaced with `TransmittableBase.SurrogateType` types
      // contract `IdenticallyTransmittable` instances
      object transformer extends Transformer {
        def underlyingType(tpe: Type): Type =
          if (tpe ne tpe.dealias)
            underlyingType(tpe.dealias)
          else if (tpe ne tpe.widen)
            underlyingType(tpe.widen)
          else
            tpe

        def originalType(tpe: Type): Option[Type] =
          surrogates collectFirst { case (surrogate, original) if tpe =:= surrogate => original }

        def restoreType(tpe: Type): Type =
          tpe map { tpe => originalType(tpe) getOrElse tpe }

        def restoreType(tree: Tree): Tree =
          if (tree.tpe != null) {
            val tpe = dealiasNonRepresentableType(tree.tpe)
            if (hasNonRepresentableType(tpe))
              internal.setType(tree, null)
            else
              internal.setType(tree, restoreType(tpe))
          }
          else
            tree

        override def transform(tree: Tree): Tree = tree match {
          case tree
            if tree.symbol != null &&
               tree.tpe != null &&
               tree.symbol.isMethod &&
               tree.tpe <:< identicallyTransmittableType =>
            q"$identicallyTransmittableTree[${restoreType(underlyingType(tree.tpe).typeArgs.head)}]()"

          case q"$resolution[..$tpts]($expr)"
            if resolution.symbol.owner == wrapperModule ||
               resolution.symbol.owner == wrapperAlternationClass =>
            transform(q"new $wrapperTree[..$tpts]($expr)")

          case TypeApply(fun, args) =>
            args foreach { restoreType(_) }
            if (args exists { _.tpe == null })
              transform(fun)
            else
              super.transform(restoreType(tree))

          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            super.transform(
              restoreType(
                treeCopy.DefDef(tree,
                  mods mapAnnotations { (tree.symbol.annotations map { _.tree }) ++ _ },
                  name, tparams, vparamss, tpt, rhs)))

          case _ =>
            super.transform(restoreType(tree))
        }
      }

      val result = c untypecheck (transformer transform resolutionTree)

      // construct `Transmittable.Resolution` instance
      // and type-check against the expected type
      val expectedResolutionType = {
        val ExistentialType(existentialQuantified, TypeRef(pre, sym, existentialArgs)) =
          typeOf[Transmittable.Resolution[_, _, _, _, _]]: @unchecked

        var quantified = List.empty[Symbol]
        var args = List.empty[Type]

        def createArg(tpe: Type, index: Int) =
          if (tpe.typeSymbol.owner.owner == resolutionModule) {
            quantified ::= existentialQuantified(index)
            args ::= existentialArgs(index)
          }
          else
            args ::= tpe

        List(T, P, R, I, B).zipWithIndex foreach (createArg _).tupled

        val typeRef = internal.typeRef(pre, sym, args)

        if (quantified.isEmpty)
          typeRef
        else
          internal.existentialType(quantified, typeRef)
      }

      val resolutionResult = c.typecheck(
        q"new ${termNames.ROOTPKG}.loci.transmitter.Transmittable.Resolution($result)",
        pt = expectedResolutionType)

      val Apply(select @ Select(New(_), _), _) = resolutionResult: @unchecked

      select foreach { tree =>
        if (tree.tpe != null)
          internal.setType(tree, dealiasNonRepresentableType(tree.tpe))
      }

      if (resolutionResult.tpe != null)
        internal.setType(resolutionResult, dealiasNonRepresentableType(resolutionResult.tpe))

      resolutionResult
    }
    else
      q"""new ${termNames.ROOTPKG}.loci.transmitter.Transmittable.Resolution[
        ${definitions.NothingTpe},
        ${definitions.NothingTpe},
        ${definitions.NothingTpe},
        ${typeOf[Future[Nothing]]},
        ${typeOf[Transmittables.None]}](
        ${termNames.ROOTPKG}.loci.transmitter.TransmittableBase.nothing)"""
  }
}
