package org.jliszka.quantum

case class Operator[A, B](f: A => Q[B]) {
  def +(g: A => Q[B]): A => Q[B] = (a: A) => f(a) + g(a)
  def -(g: A => Q[B]): A => Q[B] = (a: A) => f(a) - g(a)
  def *(z: Complex): A => Q[B] = (a: A) => f(a) * z
}
object Operator {
  implicit def functionToOperator[A, B](f: A => Q[B]): Operator[A, B] = Operator(f)
  implicit def operatorToFunction[A, B](op: Operator[A, B]): A => Q[B] = op.f
}

object Gate {
  import Q._
  import Basis._

  type U[A] = A => Q[A]

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
  def X(b: Std): Q[Std] = b match {
    case S0 => s1
    case S1 => s0
  }

  // Phase flip gate
  def Z(b: Std): Q[Std] = b match {
    case S0 => s0
    case S1 => -s1
  }

  // Hadamard gate
  def H(b: Std): Q[Std] = b match {
    case S0 => plus
    case S1 => minus
  }

  // Controlled not (CNOT) gate
  def cnot(b: T[Std, Std]): Q[T[Std, Std]] = b match {
    case T(S0, S0) => tensor(s0, s0)
    case T(S0, S1) => tensor(s0, s1)
    case T(S1, S0) => tensor(s1, s1)
    case T(S1, S1) => tensor(s1, s0)
  }

  // Rotation gate
  val tau = 2 * math.Pi
  def rot(theta: Double)(b: Std): Q[Std] = b match {
    case S0 => Q(S0 -> math.cos(theta), S1 -> math.sin(theta))
    case S1 => Q(S0 -> -math.sin(theta), S1 -> math.cos(theta))
  }

  // Square root of NOT gate
  val sqrtNot: U[Std] = rot(tau/8) _

  // Implementation of f(x) as a quantum gate
  def U(f: Int => Int, width: Int)(s: L[Std]): Q[L[Std]] = {
    val (in, out) = s.ls.splitAt(width)
    val fx = L.fromInt(f(L.toInt(L(in))) ^ L.toInt(L(out)), out.length)
    tensorLL(pure(L(in)), pure(fx))
  }

  // Tensor product of two quantum states
  def tensor[B1 <: Basis, B2 <: Basis](a: Q[B1], b: Q[B2]): Q[T[B1, B2]] = {
    for {
      x <- a
      y <- b
    } yield T(x, y)
  }

  def tensorL[B <: Basis](a: Q[B], b: Q[L[B]]): Q[L[B]] = {
    for {
      x <- a
      y <- b
    } yield L(x :: y.ls)
  }

  def tensorLL[B <: Basis](a: Q[L[B]], b: Q[L[B]]): Q[L[B]] = {
    for {
      x <- a
      y <- b
    } yield L(x.ls ++ y.ls)
  }

  // Lift 2 gates into a tensor product
  def lift12[B1 <: Basis, B1a <: Basis, B2 <: Basis, B2a <: Basis](t1: B1 => Q[B1a], t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1a, B2a]] = {
    tensor(t1(s._1), t2(s._2))
  }

  // Lift a gate into the left side of a tensor product
  def lift1[B1 <: Basis, B1a <: Basis, B2 <: Basis](t1: B1 => Q[B1a])(s: T[B1, B2]): Q[T[B1a, B2]] = {
    tensor(t1(s._1), pure(s._2))
  }

  // Lift a gate into the right side of a tensor product
  def lift2[B1 <: Basis, B2 <: Basis, B2a <: Basis](t2: B2 => Q[B2a])(s: T[B1, B2]): Q[T[B1, B2a]] = {
    tensor(pure(s._1), t2(s._2))
  }

  def liftL[B1 <: Basis, B2 <: Basis](t: B1 => Q[B2])(s: L[B1]): Q[L[B2]] = {
    s match {
      case L(Nil) => pure(L(Nil))
      case L(s0 :: rest) => tensorL(t(s0), liftL(t)(L(rest)))
    }
  }

  def liftSlice[B <: Basis](t: L[B] => Q[L[B]], start: Int, len: Int)(s: L[B]): Q[L[B]] = {
    val (pre, rest) = s.ls.splitAt(start)
    val (mid, post) = rest.splitAt(len)
    tensorLL(tensorLL(pure(L(pre)), t(L(mid))), pure(L(post)))
  }

  // Re-associate a nested tensor product
  def assoc1[B1 <: Basis, B2 <: Basis, B3 <: Basis](b: T[B1, T[B2, B3]]): Q[T[T[B1, B2], B3]] = {
    b match { case T(b1, T(b2, b3)) => pure(T(T(b1, b2), b3)) }
  }

  // Re-associate a nested tensor product the other way
  def assoc2[B1 <: Basis, B2 <: Basis, B3 <: Basis](b: T[T[B1, B2], B3]): Q[T[B1, T[B2, B3]]] = {
    b match { case T(T(b1, b2), b3) => pure(T(b1, T(b2, b3))) }
  }
  
  // Flip the two sides of tensor product
  def flip[B1 <: Basis, B2 <: Basis](b: T[B1, B2]): Q[T[B2, B1]] = {
    b match { case T(b1, b2) => pure(T(b2, b1)) }
  }
}
