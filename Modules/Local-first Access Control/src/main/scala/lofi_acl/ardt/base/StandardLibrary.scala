package lofi_acl.ardt.base

import rdts.base.Lattice

object StandardLibrary:

  // Option[T] with Some > None
  object OptionLattice:
    // TODO: This can also be derived, using the sum type Lattice
    given lattice[T: Lattice]: Lattice[Option[T]] =
      case (l, None)                    => l
      case (None, r)                    => r
      case (Some(lInner), Some(rInner)) => Some(Lattice.merge(lInner, rInner))

    given bottom[Nothing]: Bottom[Option[Nothing]] with
      override val empty: Option[Nothing] = None

  // Set[T] can be treated as a grow only set
  object GrowOnlySet:
    object mutators:
      def add[T](element: T): Set[T] = Set(element)

    given lattice[T]: Lattice[Set[T]] = (l, r) => l.union(r)

    given bottom[T]: Bottom[Set[T]] with
      override val empty: Set[T] = Set.empty

  // Map[K, V] can be treated as a grow only map, if V is mergeable (e.g., last-writer-wins register, …)
  object GrowOnlyMap:
    object mutators:
      def putMerged[K, V: Lattice](map: Map[K, V], key: K, rightValue: V): Map[K, V] =
        map.get(key) match
          case Some(leftValue) => Map.Map1(key, Lattice.merge(leftValue, rightValue))
          case None            => Map.Map1(key, rightValue)

    given lattice[K, V: Lattice]: Lattice[Map[K, V]] = (left, right) =>
      // Optimization stolen from REScalas RDT library
      val (small, large) =
        // compare unsigned treats the “unknown” value -1 as larger than any known size
        if 0 <= Integer.compareUnsigned(left.knownSize, right.knownSize)
        then (right, left)
        else (left, right)
      small.foldLeft(large) { case (current, (key, r)) =>
        current.updatedWith(key) {
          case Some(l) => Some(Lattice.merge(l, r))
          case None    => Some(r)
        }
      }

    given bottom[K, V]: Bottom[Map[K, V]] with
      override val empty: Map[K, V] = Map.empty
