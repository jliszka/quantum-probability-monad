package org.jliszka.quantum

object Examples {
  import Complex._
  import W._
  import Basis._
  import Gate._
  import Convertable._
  import Operator._
  import Util._

  def HZH(s: Q[Std]): Q[Std] = s >>= H >>= Z >>= H
  def runHZHequalsX(s: Q[Std]): (Q[Std], Q[Std]) = (HZH(s), s >>= X)

  // Some convenient states for testing
  val state1: Q[Std] = W(S0 -> 0.6, S1 -> 0.8.i)
  val state2: Q[Std] = W(S0 -> -0.5, S1 -> rquarter)

  def mkBell(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= lift1(H) >>= cnot
  def mkBell2(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= cnot >>= lift1(H)
  val bell: Q[T[Std, Std]] = mkBell(T(S0, S0))
  val bell2: Q[T[Std, Std]] = mkBell(T(S0, S1))


  def runSqrtNot = s0 >>= sqrtNot >>= sqrtNot

  // Quantum Fourier Transform
  // correct, but should be implemented in terms of smaller instances of QFT and basic gates
  def QFT(b: L[Std]): Q[L[Std]] = {
    val w = Complex.polar(1.0, tau / b.N)
    val base = w ^ L.toInt(b)
    W((0 until b.N).map(i => L.fromInt(i, b.n) -> (base ^ i)): _*).normalize
  }

  // Quantum teleportation
  def teleport(alice: Q[Std]): (Boolean, Boolean, Q[T[T[Sign, Std], Std]]) = {
    val r = tensor(alice, bell) >>= assoc1 >>= lift1(cnot) >>= lift1(lift1(convert[Std, Sign]))
    val m = r.measure(_._1)
    val bit1 = m.outcome._2 == S0
    val bit2 = m.outcome._1 == S_+
    (bit1, bit2, m.newState)
  }
  
  def receive(bit1: Boolean, bit2: Boolean, bob: Q[T[T[Sign, Std], Std]]): Q[T[T[Sign, Std], Std]] = {
    val gate1: U[Std] = if (bit1) I _ else X _
    val gate2: U[Std] = if (bit2) I _ else Z _
    bob >>= lift2(gate1) >>= lift2(gate2)
  }

  def runTeleport(alice: Q[Std]) {
    println("Alice's state: " + alice.toString)
    val (bit1, bit2, bob) = teleport(alice)
    println("Outcome of measurements: " + bit1 + ", " + bit2)
    println("Bob's state as a result of Alice's measurements: " + bob.toString)
    val r = receive(bit1, bit2, bob)
    println("Bob's state after applying gates: " + r.toString)
  }

  def runDecoherence {
    val simple = s0 >>= sqrtNot
    val entangled = bell

    println()
    println("** Without decoherence **")
    println("initial: " + simple.toString)
    val r1 = simple >>= sqrtNot
    println("rotate qubit: " + r1.toString)
    println("measure qubit: " + r1.measure().outcome)
    println("measure qubit: " + r1.measure().outcome)
    println("measure qubit: " + r1.measure().outcome)
    println("measure qubit: " + r1.measure().outcome)

    println()
    println("** With decoherence (entangled) **")
    println("initial: " + entangled.toString)
    val r2 = entangled >>= lift1(sqrtNot)
    println("rotate 1st qubit: " + r2.toString)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)
    println("measure 1st qubit: " + r2.measure(_._1).outcome)

    println()
    println("Entangled qubit behaves like a classical random bit!")
  }

  /**
   * Grover's algorithm
   */
  def grover(f: Int => Int, width: Int) = {
    val Hn = liftSlice(liftL(H), 0, width) _
    val minusL = pure(L.fromInt(1, 1)) >>= liftL(H)
    val init = tensorLL(pure(L.fromInt(0, width)), minusL) >>= Hn
    val inv = U(f, width) _
    def g(x: Int): Int = if (x == 0) 0 else 1
    // 2|s><s| - I
    // 2|s><s|k> - |k>
    val s = pure(L.fromInt(0, width))
    val g2 = (s outer s) * 2 - I
    def refl(s: L[Std]) = pure(s) >>= Hn >>= U(g, width) >>= Hn

    val r = math.Pi * math.sqrt(math.pow(2, width)) / 4
    (0 to r.toInt).foldLeft(init){ case (s, _) => s >>= inv >>= refl }
  }
  def runGrover(n: Int) = {
    def f(x: Int) = if (x == n) 1 else 0
    val bits = (math.log(n) / math.log(2)).toInt + 1
    val s = grover(f, bits)
    println("final state: " + s.toString)
    val m = L.toInt(s.measure(_.splitAt(bits)._1).outcome)
    println("measurement: " + m)
  }

  /**
   * Shor's quantum factorization algorithm (TODO)
   */
  def findPeriod(f: Int => Int, width: Int) = {
    def trial = {
      val s1 = pure(L.fromInt(0, width * 2)) >>= liftSlice(QFT, 0, width) >>= U(f, width) >>= liftSlice(QFT, 0, width)
      L.toInt(s1.measure(_.splitAt(width)._1).outcome)
    }
    def gcd(a: Int, b: Int): Int = {
      if (b == 0) a
      else gcd(b, a % b)
    }
    val r = List.fill(30)(trial).reduceLeft(gcd)
    math.pow(2, width).toInt / r
  }
  def runFindPeriod = {
    def f(x: Int) = x % 4 + 1
    findPeriod(f, 5)
  }


  /**
   * Double slit experiment
   */


  class DoubleSlit(distanceBetweenSlits: Double, distanceToScreen: Double, nDetectors: Int, distanceBetweenDetectors: Double) {

    sealed abstract class Slit(label: String) extends Basis(label)
    case object A extends Slit("A")
    case object B extends Slit("B")
    
    case class Detector(n: Int) extends Basis(n.toString)

    val emit: Q[Unit] = W(() -> one)

    def slit(q: Unit): Q[Slit] = {
      W(A -> rhalf, B -> rhalf)
    }

    def evolve(slit: Slit): Q[Detector] = {
      val slitHeight = slit match {
        case A => distanceBetweenSlits / 2
        case B => -distanceBetweenSlits / 2
      }

      val ws = for (detector <- -nDetectors to nDetectors) yield {
        val height = detector * distanceBetweenDetectors - slitHeight
        val r2 = height*height + distanceToScreen*distanceToScreen
        val distance = math.sqrt(r2)
        val amplitude = (one / r2).rot(distance)
        Detector(detector) -> amplitude
      }

      W(ws: _*)
    }

    val state: Q[Detector] = emit >>= slit >>= evolve
  }

  class QuantumEraser(distanceBetweenSlits: Double, distanceToScreen: Double, nDetectors: Int, distanceBetweenDetectors: Double) {

    sealed abstract class Slit(label: String) extends Basis(label)
    case object A extends Slit("A")
    case object B extends Slit("B")
    
    sealed abstract class Polarization(label: String) extends Basis(label)
    case object Horizontal extends Polarization("H")
    case object Vertical extends Polarization("V")
    case object Clockwise extends Polarization("C")
    case object Counterclockwise extends Polarization("G")

    case class Detector(n: Int) extends Basis(n.toString)

    val emit: Q[Polarization] = W(Horizontal -> rhalf, Vertical -> rhalf)

    def copy[S <: Basis](s: S): Q[T[S, S]] = tensor(pure(s), pure(s))

    def slit[S <: Basis](s: S): Q[T[Slit, S]] = {
      tensor(W(A -> rhalf, B -> rhalf), pure(s))
    }

    def filter(q: T[Slit, Polarization]): Q[T[Slit, Polarization]] = {
      q match {
        case T(A, Horizontal) => pure(T(A, Clockwise))
        case T(A, Vertical)   => pure(T(A, Counterclockwise))
        case T(B, Horizontal) => pure(T(B, Counterclockwise))
        case T(B, Vertical)   => pure(T(B, Clockwise))
        case _ => ???
      }
    }

    def evolve(slit: Slit): Q[Detector] = {
      val slitHeight = slit match {
        case A => distanceBetweenSlits / 2
        case B => -distanceBetweenSlits / 2
      }

      val ws = for (detector <- -nDetectors to nDetectors) yield {
        val height = detector * distanceBetweenDetectors - slitHeight
        val r2 = height*height + distanceToScreen*distanceToScreen
        val distance = math.sqrt(r2)
        val amplitude = (one / r2).rot(distance)
        Detector(detector) -> amplitude
      }

      W(ws: _*)
    }

    val state: Q[T[Detector, T[Polarization, Polarization]]] = {
      emit >>= copy >>= slit >>= assoc1 >>= lift1(filter) >>= assoc2 >>= lift1(evolve)
    }
  }

  def runQuantumEraser {
    new QuantumEraser(25, 100, 32, 5)
  }
}