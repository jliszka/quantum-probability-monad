package org.jliszka.quantum

trait Convertable[B1 <: Basis, B2 <: Basis] {
  def convert(b: B1): W.Q[B2]
}

object Convertable {
  import W._
  import Basis._

  implicit object SignIsConvertable extends Convertable[Std, Sign] {
    def convert(b: Std): Q[Sign] = b match {
      case S0 => W(S_+ -> rhalf, S_- -> rhalf)
      case S1 => W(S_+ -> rhalf, S_- -> -rhalf)
    }
  }

  implicit def id[B <: Basis]: Convertable[B, B] = new Convertable[B, B] {
    override def convert(b: B): Q[B] = pure(b)
  }

  implicit def sym[B1 <: Basis, B2 <: Basis](implicit from: Convertable[B2, B1], enum: Enumerable[B2]): Convertable[B1, B2] = new Convertable[B1, B2] {
    override def convert(b1: B1): Q[B2] = {
      W(enum.vectors.map(b2 => b2 -> from.convert(b2)(b1).conj): _*)
    }
  }

  implicit def TIsConvertable[B1 <: Basis, B2 <: Basis, B3 <: Basis, B4 <: Basis](implicit c1: Convertable[B1, B3], c2: Convertable[B2, B4]): Convertable[T[B1, B2], T[B3, B4]] = new Convertable[T[B1, B2], T[B3, B4]] {
    def convert(b: T[B1, B2]) = pure(b) >>= Gate.lift12(c1.convert, c2.convert)
  }
  
/*
  implicit def trans[B1 <: Basis, B2 <: Basis, B3 <: Basis](implicit c12: Convertable[B1, B2], c23: Convertable[B2, B3]): Convertable[B1, B3] = new Convertable[B1, B3] {
    override def convert(b1: B1): Q[B3] = pure(b1) >>= c12.convert >>= c23.convert
  }
*/

  def convert[B1 <: Basis, B2 <: Basis](implicit c: Convertable[B1, B2]): B1 => Q[B2] = c.convert _
}

trait Enumerable[B <: Basis] {
  def vectors: List[B]
}

object Enumerable {
  import Basis._

  implicit object StdIsEnumerable extends Enumerable[Std] {
    override val vectors = List(S0, S1)
  }

  implicit def TIsEnumerable[B1 <: Basis, B2 <: Basis](implicit e1: Enumerable[B1], e2: Enumerable[B2]): Enumerable[T[B1, B2]] = new Enumerable[T[B1, B2]] {
    override val vectors = {
      for {
        b1 <- e1.vectors
        b2 <- e2.vectors
      } yield T(b1, b2)
    }
  }  
}
