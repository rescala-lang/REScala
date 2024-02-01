package loci
package transmitter

import Transmittables.{Delegates, Message}

import scala.annotation.implicitNotFound
import scala.compiletime.ops.int

@implicitNotFound("Transmittable.Any[${B}, ${I}, ${R}] not specified in: ${S}")
sealed trait Selector[B, I, R, P, T <: Transmittables, S <: Transmittables]:
  def transmittable(transmittables: S): Transmittable.Aux[B, I, R, P, T]
  def context(contexts: Contexts[S]): ContextBuilder.Context[T]
  def contextBuilder(contextBuilders: ContextBuilders[S]): ContextBuilder[T]

object Selector:
  type Selected[S <: Transmittables, N <: Int] = N match
    case 0 => S match
      case Message[t] => t
      case Delegates[d] => d match
        case d / t => t
        case _ => d
    case int.S[n] => S match
      case Delegates[d / t] => Selected[Delegates[d], n]

  final class IndexFallback[B, I, R, P, T <: Transmittables, S <: Transmittables, V]

  object IndexFallback:
    transparent inline given message[B, I, R, P, T <: Transmittables]
      : IndexFallback[B, I, R, P, T, Message[Transmittable.Aux[B, I, R, P, T]], Transmittable.Aux[B, I, R, P, T]] = ${ ??? }
    transparent inline given single[B, I, R, P, T <: Transmittables]
      : IndexFallback[B, I, R, P, T, Delegates[Transmittable.Aux[B, I, R, P, T]], Transmittable.Aux[B, I, R, P, T]] = ${ ??? }
    transparent inline given head[B, I, R, P, T <: Transmittables, D <: Transmittable.Delegating]
      : IndexFallback[B, I, R, P, T, Delegates[D / Transmittable.Aux[B, I, R, P, T]], Transmittable.Aux[B, I, R, P, T]] = ${ ??? }
    transparent inline given tail[B, I, R, P, T <: Transmittables, B0, I0, R0, P0, T0 <: Transmittables, D <: Transmittable.Delegating](using
        IndexFallback[B, I, R, P, T, Delegates[D], Transmittable.Aux[B, I, R, P, T]])
      : IndexFallback[B, I, R, P, T, Delegates[D / Transmittable.Aux[B0, I0, R0, P0, T0]], Transmittable.Aux[B, I, R, P, T]] = ${ ??? }


  sealed trait Base[B, I, R, P, T <: Transmittables, S <: Transmittables]
    extends Selector[B, I, R, P, T, S]

  sealed trait BaseFallback:
    transparent inline given [B, I, R, P, T <: Transmittables, S <: Transmittables](using
        IndexFallback[B, I, R, P, T, S, Transmittable.Aux[B, I, R, P, T]])
      : Base[B, I, R, P, T, S] = ${ ??? }

  object Base extends BaseFallback:
    def apply[B, I, R, P, T <: Transmittables, S <: Transmittables, N <: Int](using
        Selected[S, N] <:< Transmittable.Aux[B, I, R, P, T],
        ValueOf[N])
      : Base[B, I, R, P, T, S] = Impl(valueOf[N])

    transparent inline given [B, I, R, P, T <: Transmittables, S <: Transmittables, N <: Int](using
        inline index: SelectorResolution.Index[Message, Delegates, /, SelectorResolution.TypeMember[Transmittable.Any[?, ?, ?]#Base], B, S, N],
        inline selected: Selected[S, N] <:< Transmittable.Aux[B, I, R, P, T],
        inline value: ValueOf[N])
      : Base[B, I, R, P, T, S] = apply[B, I, R, P, T, S, N]


  sealed trait Intermediate[B, I, R, P, T <: Transmittables, S <: Transmittables]
    extends Selector[B, I, R, P, T, S]

  sealed trait IntermediateFallback:
    transparent inline given [B, I, R, P, T <: Transmittables, S <: Transmittables](using
        IndexFallback[B, I, R, P, T, S, Transmittable.Aux[B, I, R, P, T]])
      : Intermediate[B, I, R, P, T, S] = ${ ??? }

  object Intermediate extends IntermediateFallback:
    def apply[B, I, R, P, T <: Transmittables, S <: Transmittables, N <: Int](using
        Selected[S, N] =:= Transmittable.Aux[B, I, R, P, T],
        ValueOf[N])
      : Intermediate[B, I, R, P, T, S] = Impl(valueOf[N])

    transparent inline given [B, I, R, P, T <: Transmittables, S <: Transmittables, N <: Int](using
        inline index: SelectorResolution.Index[Message, Delegates, /, SelectorResolution.TypeMember[Transmittable.Any[?, ?, ?]#Intermediate], I, S, N],
        inline selected: Selected[S, N] =:= Transmittable.Aux[B, I, R, P, T],
        inline value: ValueOf[N])
      : Intermediate[B, I, R, P, T, S] = apply[B, I, R, P, T, S, N]


  private class Impl[B, I, R, P, T <: Transmittables, S <: Transmittables](index: Int)
      extends Base[B, I, R, P, T, S] with Intermediate[B, I, R, P, T, S]:
    def transmittable(transmittables: S) = transmittable(transmittables, index)
    def context(contexts: Contexts[S]) = context(contexts, index)
    def contextBuilder(contextBuilders: ContextBuilders[S]) = contextBuilder(contextBuilders, index)

    def transmittable(transmittables: Transmittables, index: Int): Transmittable.Aux[B, I, R, P, T] =
      transmittables: @unchecked match
        case transmittables: Message[Transmittable.Aux[B, I, R, P, T]] @unchecked =>
          transmittables.message
        case transmittables: Delegates[?] =>
          transmittables.delegates: @unchecked match
            case transmittable: Transmittable.Aux[B, I, R, P, T] @unchecked =>
              transmittable
            case delegates: (? / Transmittable.Aux[B, I, R, P, T]) @unchecked =>
              if index <= 0 then delegates.head
              else transmittable(delegates.tailDelegates, index - 1)

    def context(contexts: Contexts[?], index: Int): ContextBuilder.Context[T] =
      contexts: @unchecked match
        case single: Contexts.SingleMessage[B, I, R, P, T] @unchecked =>
          single.context
        case single: Contexts.SingleDelegate[B, I, R, P, T] @unchecked =>
          single.context
        case list: Contexts.List[B, I, R, P, T, ?] @unchecked =>
          if index <= 0 then list.contextHead
          else context(list.contextTail, index - 1)

    def contextBuilder(contextBuilders: ContextBuilders[?], index: Int): ContextBuilder[T] =
      contextBuilders: @unchecked match
        case single: ContextBuilders.SingleMessage[B, I, R, P, T] @unchecked =>
          single.contextBuilder
        case single: ContextBuilders.SingleDelegate[B, I, R, P, T] @unchecked =>
          single.contextBuilder
        case list: ContextBuilders.List[B, I, R, P, T, ?] @unchecked =>
          if (index <= 0) list.contextBuilderHead
          else contextBuilder(list.contextBuilderTail, index - 1)
  end Impl
end Selector
