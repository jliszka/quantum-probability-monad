package org.jliszka.quantum

case class W[A, B](state: (A, B)*)(implicit num: Numeric[B], num2: Numeric2[B], ord: Ordering[A] = null) {
  private val rand = new scala.util.Random()
  import num._
  private val _m = state.toMap
  def apply(a: A): B = _m.getOrElse(a, num.zero)

  def map[A1](f: A => A1)(implicit ord1: Ordering[A1] = null): W[A1, B] = {
    W(state.map{ case (a, b) => (f(a), b) }: _*).collect.normalize
  }

  def mapV[B1: Numeric: Numeric2](f: B => B1): W[A, B1] = {
    W(state.map{ case (a, b) => (a, f(b)) }: _*)
  }

  def flatMap[A1](f: A => W[A1, B]): W[A1, B] = {
    W(state.flatMap{ case (a, b) => f(a).mapV(_ * b).state }: _*).collect.normalize
  }

  def >>=[A1](f: A => W[A1, B]): W[A1, B] = this.flatMap(f)

  // Collect like terms and sum their coefficients
  def collect: W[A, B] = {
    W(state.groupBy(_._1).toList.map{ case (a1, abs) => (a1, abs.map(_._2).sum) }: _*)
  }

  def scale(x: Double) = {
    W(state.map{ case (a, b) => (a, num2.scale(b, x)) }: _*)
  }
  def unary_- = this.scale(-1.0)

  def *(x: B) = this.mapV(_ * x)
  def +(that: W[A, B]) = W(this.state ++ that.state :_*).collect
  def -(that: W[A, B]) = this + that.scale(-1.0)

  def filter(f: A => Boolean): W[A, B] = {
    W(state.filter{ case (a, b) => f(a) }: _*).normalize
  }

  // Make sure the sum of the squares of the coefficients is 1
  def normalize = {
    val total = math.sqrt(state.map{ case (a, b) => num2.norm(b) }.sum)
    this.scale(1 / total)
  }

  def toDist: List[(A, Double)] = {
    this.state.toList.map{ case (a, b) => a -> num2.norm(b) }
  }

  def hist(implicit ord: Ordering[A]) {
    plotHist(this.toDist)
  }

  private def plotHist[A1](values: Seq[(A1, Double)])(implicit ord: Ordering[A1]) {
    val maxWidth = values.map(_._1.toString.size).max
    val maxValue = values.map(_._2).max
    val hashesPerUnit = 50 / maxValue
    values.sortBy(_._1).foreach{ case (a, p) => {
      val fmt = "%"+maxWidth+"s %s"
      val hashes = (hashesPerUnit * p).toInt
      println(fmt.format(a, "#" * hashes))
    }}    
  }

  case class Measurement[A, B, C](outcome: A, newState: W[B, C])

  // Measure a quantum state (or a part of one). Returns the outcome of the measurement and the new state.
  def measure[A1](w: A => A1 = identity[A] _): Measurement[A1, A, B] = {
    val r = rand.nextDouble()
    def find(r: Double, s: List[(A, Double)]): A = s match {
      case (l, p) :: Nil => l
      case (l, p) :: rest if r < p => l
      case (l, p) :: rest => find(r - p, rest)
      case Nil => throw new Exception("empty state")
    }
    val outcome = w(find(r, this.toDist))
    val newState = this.filter(s => w(s) == outcome)
    Measurement(outcome, newState)
  }

  def simulate[A1](n: Int, w: A => A1 = identity[A] _)(implicit ord: Ordering[A1]) {
    val measurements = (1 to n).map(_ => this.measure(w).outcome).groupBy(x => x).mapValues(_.size.toDouble)
    val basis = this.state.map{ case (a, b) => w(a) }.distinct
    plotHist(basis.map(b => b -> measurements.getOrElse(b, 0.0)))
  }

  def inner(other: W[A, B]) = {
    val m = other.state.toMap
    (for {
      (l, v1) <- state
      v2 <- m.get(l)
    } yield num2.conj(v1) * v2).sum
  }

  def outer(other: W[A, B]) = {
    val m = other.state.toMap
    (a: A) => this * num2.conj(m.getOrElse(a, num.zero))
  }

  override def toString = {
    val filtered = state.filter{ case (a, b) => num2.norm(b) > 0.00000001 }
    val sorted = if (ord == null) filtered.sortBy{ case (a, b) => a.toString } else filtered.sortBy{ case (a, b) => a }
    sorted.map{ case (a, b) => s"$b|$a>"}.mkString(" + ")
  }
}

object W {
  type Q[A] = W[A, Complex]
  val rhalf: Complex = math.sqrt(0.5)
  val rquarter: Complex = math.sqrt(0.25)
  def pure[A](a: A): Q[A] = new W(a -> Complex.one)
}




