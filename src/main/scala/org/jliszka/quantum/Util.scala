package org.jliszka.quantum

object Util {
  import Q._
  import Gate.U
  import Basis._
  import Util._

  // Find the eigenvalues of an unitary operator
  def eigen(u: U[Std]) = {
    def solve(a: Complex, b: Complex, c: Complex): (Complex, Complex) = {
      val det = (b*b - 4*a*c)^(0.5)
      ((-b + det) / (2*a), (-b - det) / (2*a))
    }
    val a = u(S0)(S0)
    val b = u(S1)(S0)
    val c = u(S0)(S1)
    val d = u(S1)(S1)
    val (e1, e2) = solve(1, -(a+d), a*d - b*c)
    (e1, e2)
  }

  // Find the adjoint of a unitary transformation
  def adjoint[B1 <: Basis, B2 <: Basis, B3 <: Basis](u: B1 => Q[B2])(implicit c31: Convertable[B3, B1], c23: Convertable[B2, B3], e3: Enumerable[B3]): B2 => Q[B1] = {
    def adjointEnum(u: B3 => Q[B3])(b3: B3): Q[B3] = {
      val basis = e3.vectors
      Q(basis.map(b => b -> u(b)(b3).conj): _*)
    }
    (b2: B2) => pure(b2) >>= c23.convert >>= adjointEnum({ (b3: B3) => pure(b3) >>= c31.convert >>= u >>= c23.convert }) >>= c31.convert
  }    
}