package org.jliszka.quantum

import org.jliszka.quantum.Basis.{L, T}

case class Q[A <: Basis](state: (A, Complex)*)(implicit ord: Ordering[A] = null) {
  private val rand = new scala.util.Random()
  private val _m = state.toMap
  
  def apply(a: A): Complex = _m.getOrElse(a, Complex.zero)

  private def map[B <: Basis](f: A => B)(implicit ord1: Ordering[B] = null): Q[B] = {
    Q(state.map{ case (a, z) => (f(a), z) }: _*).collect.normalize
  }

  private def mapV(f: Complex => Complex): Q[A] = {
    Q(state.map{ case (a, z) => (a, f(z)) }: _*)
  }

  // Collect like terms and sum their coefficients
  private def collect: Q[A] = {
    Q(state.groupBy(_._1).toList.map{ case (a, azs) => (a, azs.map(_._2).sum) }: _*)
  }

  def flatMap[B <: Basis](f: A => Q[B]): Q[B] = {
    Q(state.flatMap{ case (a, z) => f(a).mapV(_ * z).state }: _*).collect
  }

  def >>=[B <: Basis](f: A => Q[B]): Q[B] = this.flatMap(f)

  def unary_- = this * -1.0

  def *(z: Complex) = this.mapV(_ * z)
  def /(z: Complex) = this.mapV(_ / z)
  def +(that: Q[A]) = Q(this.state ++ that.state :_*).collect
  def -(that: Q[A]) = this + -that

  // Inner product
  def inner(that: Q[A]): Complex = {
    this.state.map{ case (l, v) => v.conj * that(l) }.sum
  }
  def <>(that: Q[A]): Complex = this.inner(that)

  // Outer product
  def outer[B <: Basis](that: Q[B]): B => Q[A] = {
    (b: B) => this * that(b).conj
  }
  def ><[B <: Basis](that: Q[B]): B => Q[A] = this.outer(that)

  // Tensor products
  def *[B <: Basis](that: Q[B]): Q[T[A, B]] = {
    for {
      x <- this
      y <- that
    } yield T(x, y)
  }
  def ⊗[B <: Basis](that: Q[B]): Q[T[A, B]] = this * that

  def *:[B <: Basis](that: Q[B])(implicit ev: A =:= L[B]): Q[L[B]] = {
    for {
      x <- that
      y <- this
    } yield L(x :: ev(y).ls)
  }

  def **[B <: Basis](that: Q[L[B]])(implicit ev: A =:= L[B]): Q[L[B]] = {
    for {
      x <- this
      y <- that
    } yield L(ev(x).ls ++ y.ls)
  }

  private def filter(f: A => Boolean): Q[A] = {
    Q(state.filter{ case (a, z) => f(a) }: _*).normalize
  }

  // Make sure the sum of the squares of the coefficients is 1
  def normalize = {
    val total = math.sqrt(state.map{ case (a, z) => z.norm2 }.sum)
    this / total
  }

  def toDist: List[(A, Double)] = {
    this.state.toList.map{ case (a, z) => a -> z.norm2 }
  }

  def hist(implicit ord: Ordering[A]) {
    plotHist(this.toDist)
  }

  private def plotHist[B](values: Seq[(B, Double)])(implicit ord: Ordering[B]) {
    val maxWidth = values.map(_._1.toString.size).max
    val maxValue = values.map(_._2).max
    val hashesPerUnit = 50 / maxValue
    values.sortBy(_._1).foreach{ case (a, p) => {
      val fmt = "%"+maxWidth+"s %s"
      val hashes = (hashesPerUnit * p).toInt
      println(fmt.format(a, "#" * hashes))
    }}    
  }

  case class Measurement[A <: Basis, B](outcome: B, newState: Q[A])

  // Measure a quantum state (or a part of one). Returns the outcome of the measurement and the new state.
  def measure[B](w: A => B = identity[A] _): Measurement[A, B] = {
    val dist = this.toDist
    val total = dist.map(_._2).sum
    val r = rand.nextDouble() * total
    def find(r: Double, s: List[(A, Double)]): A = s match {
      case (l, p) :: Nil => l
      case (l, p) :: rest if r < p => l
      case (l, p) :: rest => find(r - p, rest)
      case Nil => throw new Exception("empty state")
    }
    val outcome = w(find(r, dist))
    val newState = this.filter(s => w(s) == outcome)
    Measurement(outcome, newState)
  }

  def plotMeasurements[B](n: Int, w: A => B = identity[A] _)(implicit ord: Ordering[B]) {
    val measurements = (1 to n).map(_ => this.measure(w).outcome).groupBy(x => x).mapValues(_.size.toDouble)
    val basis = this.state.map{ case (a, z) => w(a) }.distinct
    plotHist(basis.map(b => b -> measurements.getOrElse(b, 0.0)))
  }

  override def toString = {
    val filtered = state.filter{ case (a, z) => z.norm2 > 0.00000001 }
    if (filtered.isEmpty) "0"
    else {
      val sorted = if (ord == null) filtered.sortBy{ case (a, z) => a.toString } else filtered.sortBy{ case (a, z) => a }
      sorted.map{ case (a, z) => {
        val zStr = z.toString
        val zDisplay = if (zStr == "1") "" else zStr
        s"$zDisplay|$a>"
      }}.mkString(" + ")
    }
  }
}

object Q {
  val rhalf: Complex = math.sqrt(0.5)
  val rquarter: Complex = math.sqrt(0.25)
  val r3quarters: Complex = math.sqrt(0.75)
  def pure[A <: Basis](a: A): Q[A] = new Q(a -> Complex.one)
}
