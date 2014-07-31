package org.jliszka.quantum

import org.jliszka.quantum.Q._
import org.jliszka.quantum.Basis._


final class Operator[A, B <: Basis](val f: A => Q[B]) extends AnyVal {
  def +(g: A => Q[B]): A => Q[B] = (a: A) => f(a) + g(a)
  def -(g: A => Q[B]): A => Q[B] = (a: A) => f(a) - g(a)
  def *(z: Complex): A => Q[B] = (a: A) => f(a) * z
  def >=>[C <: Basis](g: B => Q[C]): A => Q[C] = (a: A) => f(a) >>= g
}

object Operator {
  implicit def functionToOperator[A, B <: Basis](f: A => Q[B]): Operator[A, B] = new Operator(f)
  implicit def operatorToFunction[A, B <: Basis](op: Operator[A, B]): A => Q[B] = op.f
  def repeat[A <: Basis](n: Int)(f: A => Q[A]): A => Q[A] = (a: A) => {
    @scala.annotation.tailrec
    def helper(n: Int, acc: Q[A]): Q[A] = {
      if (n <= 0) acc
      else helper(n-1, acc >>= f)
    }
    helper(n, Q.pure(a))
  }  
}

object Gate {
  import Operator._

  // The type of a unitary transformation
  type U[A <: Basis] = A => Q[A]

  // Some pure states
  val s0: Q[Std] = pure(S0)
  val s1: Q[Std] = pure(S1)

  val plus: Q[Std] = Q(S0 -> rhalf, S1 -> rhalf)
  val minus: Q[Std] = Q(S0 -> rhalf, S1 -> -rhalf)

  val s_+ = pure(S_+)
  val s_- = pure(S_-)

  // Identity gate
  def I[B <: Basis](b: B): Q[B] = pure(b)

  // Not gate
  val X: U[Std] = (s0 >< s1) + (s1 >< s0)

  // Phase flip gate
  val Z: U[Std] = (s0 >< s0) + (-s1 >< s1)

  // Hadamard gate
  val H: U[Std] = (plus >< s0) + (minus >< s1)

  def controlled[B <: Basis](g: B => Q[B]): T[Std, B] => Q[T[Std, B]] = (s: T[Std, B]) => s match {
    case T(S0, b) => pure(T(S0, b))
    case T(S1, b) => s1 * g(b)
  }

  // Controlled not (CNOT) gate
  val cnot: U[T[Std, Std]] = controlled(X)

  def R(theta: Double): U[Std] = (s0 >< s0) + (s1 * Complex.one.rot(theta) >< s1)

  // Rotation gate
  val tau = 2 * math.Pi
  def rot(theta: Double): U[Std] = {
    val s0a = s0 * math.cos(theta) + s1 * math.sin(theta)
    val s1a = s0 * -math.sin(theta) + s1 * math.cos(theta)
    (s0a >< s0) + (s1a >< s1)
  }

  // Square root of NOT gate
  val sqrtNot: U[Std] = rot(tau/8)

  // Implementation of f(x) as a quantum gate
  def U(f: Int => Int): T[L[Std], L[Std]] => Q[T[L[Std], L[Std]]] = (s: T[L[Std], L[Std]]) => {
    val T(x, out) = s
    val fx = L.fromInt(f(L.toInt(x)) ^ L.toInt(out), out.ls.length)
    pure(x) * pure(fx)
  }


  /**
   * Wire manipulation gates
   */

  // Lift 2 gates into a tensor product
  def lift12[B1 <: Basis, B1a <: Basis, B2 <: Basis, B2a <: Basis](t1: B1 => Q[B1a], t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1a, B2a]] = {
    t1(s._1) * t2(s._2)
  }

  // Lift a gate into the left side of a tensor product
  def lift1[B1 <: Basis, B1a <: Basis, B2 <: Basis](t1: B1 => Q[B1a])(s: T[B1, B2]): Q[T[B1a, B2]] = {
    t1(s._1) * pure(s._2)
  }

  // Lift a gate into the right side of a tensor product
  def lift2[B1 <: Basis, B2 <: Basis, B2a <: Basis](t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1, B2a]] = {
    pure(s._1) * t2(s._2)
  }

  // Re-associate a nested tensor product
  def assoc1[B1 <: Basis, B2 <: Basis, B3 <: Basis](b: T[B1, T[B2, B3]]): Q[T[T[B1, B2], B3]] = {
    b match { case T(b1, T(b2, b3)) => pure(T(T(b1, b2), b3)) }
  }

  // Re-associate a nested tensor product the other way
  def assoc2[B1 <: Basis, B2 <: Basis, B3 <: Basis](b: T[T[B1, B2], B3]): Q[T[B1, T[B2, B3]]] = {
    b match { case T(T(b1, b2), b3) => pure(T(b1, T(b2, b3))) }
  }
  
  // Swap the two sides of tensor product
  def swap[B1 <: Basis, B2 <: Basis](b: T[B1, B2]): Q[T[B2, B1]] = {
    b match { case T(b1, b2) => pure(T(b2, b1)) }
  }  

  // List manipulation
  def liftL[B1 <: Basis, B2 <: Basis](t: B1 => Q[B2])(s: L[B1]): Q[L[B2]] = {
    s match {
      case L(Nil) => pure(L(Nil))
      case L(h :: rest) => t(h) *: liftL(t)(L(rest))
    }
  }

  def liftHead[B <: Basis](t: B => Q[B])(s: L[B]): Q[L[B]] = {
    s match {
      case L(Nil) => pure(L(Nil))
      case L(h :: rest) => t(h) *: pure(L(rest))
    }
  }

  def liftTail[B <: Basis](t: L[B] => Q[L[B]])(s: L[B]): Q[L[B]] = {
    s match {
      case L(Nil) => pure(L(Nil))
      case L(h :: rest) => pure(h) *: t(L(rest))
    }
  }

  def splitAt[B <: Basis](n: Int)(xs: L[B]): Q[T[L[B], L[B]]] = {
    val (a, b) = xs.ls.splitAt(n)
    pure(T(L(a), L(b)))
  }

  def join[B <: Basis](xs: T[L[B], L[B]]): Q[L[B]] = {
    val T(L(a), L(b)) = xs
    pure(L(a ++ b))
  }

  def reverse[B <: Basis](s: L[B]): Q[L[B]] = {
    pure(L(s.ls.reverse))
  }

  def cons[B <: Basis](s: T[B, L[B]]): Q[L[B]] = s match {
    case T(h, L(t)) => pure(L(h :: t))
  }

  def decons[B <: Basis](s: L[B]): Q[T[B, L[B]]] = s match {
    case L(h :: t) => pure(T(h, L(t)))
  }

  def liftSlice[B <: Basis](t: L[B] => Q[L[B]], start: Int, len: Int)(s: L[B]): Q[L[B]] = {
    val (pre, rest) = s.ls.splitAt(start)
    val (mid, post) = rest.splitAt(len)
    pure(L(pre)) ** t(L(mid)) ** pure(L(post))
  }
}
