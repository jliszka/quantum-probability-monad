package org.jliszka.quantum

abstract class Basis(val label: String) {
  override def toString = label
}

object Basis {

  implicit def basisOrdering[B <: Basis] = Ordering.by[B, String](_.label)

  // Standard basis { |0>, |1> }
  abstract sealed class Std(label: String) extends Basis(label)
  case object S0 extends Std("0")
  case object S1 extends Std("1")

  // Sign basis { |+>, |-> }
  abstract sealed class Sign(label: String) extends Basis(label)
  case object S_+ extends Sign("+")
  case object S_- extends Sign("-")


  // Tensor product of two bases, e.g., T[Std, Std] = { |00>, |01>, |10>, |11> }
  case class T[+B1 <: Basis, +B2 <: Basis](_1: B1, _2: B2) extends Basis(_1.label + "," + _2.label)

  implicit def tensorOrdering[B1 <: Basis, B2 <: Basis](implicit ord1: Ordering[B1], ord2: Ordering[B2]): Ordering[T[B1, B2]] = {
    new Ordering[T[B1, B2]] {
      def compare(x: T[B1, B2], y: T[B1, B2]) = {
        ord1.compare(x._1, y._1) match {
          case 0 => ord2.compare(x._2, y._2)
          case a => a
        }
      }
    }
  }

  case class L[B <: Basis](ls: List[B]) extends Basis(ls.map(_.label).mkString) {
    val n = ls.length
    val N = math.pow(2, n).toInt
    def splitAt(n: Int) = {
      val (a, b) = ls.splitAt(n)
      (L(a), L(b))
    }
  }

  object L {
    def fromInt(i: Int, width: Int): L[Std] = {
      def helper(i: Int, width: Int, acc: List[Std]): List[Std] = {
        if (width == 0) acc
        else helper(i / 2, width-1, (if (i % 2 == 0) S0 else S1) :: acc)
      }
      new L(helper(i, width, Nil))
    }

    def toInt(s: L[Std]): Int = {
      def helper(ls: List[Std], acc: Int): Int = {
        ls match {
          case Nil => acc
          case S0 :: rest => helper(rest, acc * 2)
          case S1 :: rest => helper(rest, acc * 2 + 1)
        }
      }
      helper(s.ls, 0)
    }
  }
}
