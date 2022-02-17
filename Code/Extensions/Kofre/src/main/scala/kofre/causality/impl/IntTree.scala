package kofre.causality.impl

import kofre.Lattice

object IntTree {

  type Num = Long

  sealed trait Tree

  /** inclusive exclusive ranges */
  case class Range(from: Num, until: Num, less: Tree, more: Tree) extends Tree
  case object Empty                                               extends Tree

  implicit val lattice: Lattice[Tree] = IntTree.merge _

  def fromIterator(iterable: Iterator[Num]): Tree = iterable.foldLeft(empty)(IntTree.insert)

  def show(tree: Tree): String =
    tree match {
      case Empty       => "[]"
      case tree: Range => s"{L${show(tree.less)} I[${tree.from}, ${tree.until - 1}] R${show(tree.more)}}"
    }

  val empty: Tree = Empty

  def insert(tree: Tree, value: Num): Tree = insert(tree, value, value + 1)

  def ranges(tree: Tree): Iterator[Range] =
    tree match {
      case Empty       => Iterator.empty
      case tree: Range => (ranges(tree.less) ++ Iterator.single(tree)) ++ ranges(tree.more)
    }

  def toSeq(tree: Tree): List[Num] = iterator(tree).toList

  def iterator(tree: Tree): Iterator[Num] = tree match {
    case Empty       => Iterator.empty[Num]
    case tree: Range => iterator(tree.less) ++ (tree.from until tree.until) ++ iterator(tree.more)
  }

  def foldLeft[Z](tree: Tree, z: Z)(f: (Z, Num, Num) => Z): Z =
    tree match
      case Empty => z
      case Range(from, until, less, more) =>
        val left   = foldLeft(less, z)(f)
        val middle = f(left, from, until)
        foldLeft(more, middle)(f)

  def merge(left: Tree, right: Tree): Tree = foldLeft(right, left) { (ir, from, until) => insert(ir, from, until) }

  private def overlap(start: Num, middle: Num, end: Num): Boolean = start <= middle && middle <= end

  def nextValue(tree: Tree, default: Num): Num =
    max(tree) match {
      case (_, maxr: Range) => maxr.until
      case _                => default
    }

  /** returns the tree range without the max, and the max node */
  def max(tree: Tree): (Tree, Tree) =
    tree match {
      case Empty => (tree, tree)
      case tree: Range =>
        import tree.*
        more match {
          case more: Range =>
            val (t, m) = max(more)
            (copy(more = t), m)
          case Empty => (less, tree)
        }
    }

  /** returns the tree range without the min, and the min node */
  def min(tree: Tree): (Tree, Tree) =
    tree match {
      case Empty => (tree, tree)
      case tree: Range =>
        import tree.*
        less match {
          case less: Range =>
            val (t, m) = min(less)
            (copy(less = t), m)
          case Empty => (more, tree)
        }
    }

  @scala.annotation.tailrec
  private def flatten(tree: Tree): Tree =
    tree match {
      case Empty => tree
      case tree: Range =>
        import tree.*
        max(less) match {
          case (lesserOther, lesserMax: Range) if from <= lesserMax.until =>
            flatten(Range(math.min(from, lesserMax.from), math.max(until, lesserMax.until), lesserOther, more))

          case _ =>
            min(more) match {
              case (upd, morere: Range) if morere.from <= until =>
                flatten(Range(math.min(from, morere.from), math.max(until, morere.until), less, upd))

              case _ => tree
            }
        }
    }

  def insert(tree: Tree, iFrom: Num, iUntil: Num): Tree =
    tree match {
      case Empty => Range(iFrom, iUntil, Empty, Empty)
      case Range(from, until, less, more) =>
        if (iUntil < from) Range(from, until, insert(less, iFrom, iUntil), more)
        else if (until < iFrom) Range(from, until, less, insert(more, iFrom, iUntil))
        else flatten(Range(math.min(from, iFrom), math.max(iUntil, until), less, more))
    }

  @scala.annotation.tailrec
  def contains(tree: Tree, search: Num): Boolean =
    tree match {
      case Empty => false
      case tree: Range => {
        if (search < tree.from) contains(tree.less, search)
        else if (tree.until <= search) contains(tree.more, search)
        else true
      }
    }

  /** this was an experiment in performance optimization which seems to have not worked */
  private object CatsDietLike {
    def noMoreThan(tree: Tree, a: Num): (Tree, Num) =
      tree match {
        case Range(start, end, l, r) =>
          if (a > end) {
            val (r2, a2) = noMoreThan(r, a)
            (Range(start, end, l, r2), math.min(a, a2))
          } else if ((a >= start)) (l, start)
          else noMoreThan(l, a)
        case Empty => (Empty, a)
      }

    def noLessThan(tree: Tree, a: Num): (Tree, Num) =
      tree match {
        case Range(start, end, l, r) =>
          if ((a < (start - 1))) {
            val (l2, a2) = noLessThan(l, a)
            (Range(start, end, l2, r), math.max(a, a2))
          } else if ((a < end)) (r, end)
          else noLessThan(r, a)
        case Empty => (Empty, a)
      }

    def addRanges(from1: Num, until1: Num, from2: Num, until2: Num): ((Num, Num), (Num, Num)) = {
      if overlap(from1, from2, until1) || overlap(from2, from1, until2) then
        ((math.max(from1, from2), math.max(until1, until2)), (0, 0))
      else if (from1 < from2) then ((from1, until1), (from2, until2))
      else ((from2, until2), (from1, until1))
    }

    def addRangeIncreasing(tree: Tree, iFrom: Num, iUntil: Num): Tree =
      tree match {
        case Empty => Range(iFrom, iUntil, Empty, Empty)

        case Range(from, until, l, r) =>
          val (r1, r2) = addRanges(from, until, iFrom, iUntil)
          r2 match {
            case (0, 0) =>
              val (left, start) = noMoreThan(l, r1._1)
              val (right, end)  = noLessThan(r, r1._2)
              Range(start, end, left, right)

            case (fr, un) =>
              if (r1 == (from, until))
                Range(r1._1, r1._2, l, addRangeIncreasing(r, r2._1, r2._2))
              else
                Range(r2._1, r2._2, addRangeIncreasing(l, r1._1, r2._2), r)
          }
      }
  }
}
