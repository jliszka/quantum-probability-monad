package org.jliszka.quantum

import org.jliszka.quantum.Complex._
import org.jliszka.quantum.Q._
import org.jliszka.quantum.Basis._
import org.jliszka.quantum.Gate._
import org.jliszka.quantum.Convertable._
import org.jliszka.quantum.Operator._
import org.jliszka.quantum.Util._

object Examples {

  def HZH(s: Q[Std]): Q[Std] = s >>= H >>= Z >>= H
  def runHZHequalsX(s: Q[Std]): (Q[Std], Q[Std]) = (HZH(s), s >>= X)

  // Some convenient states for testing
  val state1: Q[Std] = Q(S0 -> 0.6, S1 -> 0.8.i)
  val state2: Q[Std] = Q(S0 -> -0.5, S1 -> r3quarters)

  def mkBell(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= lift1(H) >>= cnot
  def mkBell2(s: T[Std, Std]): Q[T[Std, Std]] = pure(s) >>= cnot >>= lift1(H)
  val bell: Q[T[Std, Std]] = mkBell(T(S0, S0))
  val bell2: Q[T[Std, Std]] = mkBell(T(S0, S1))


  def runSqrtNot = s0 >>= sqrtNot >>= sqrtNot

  // Quantum teleportation
  def teleport(alice: Q[Std]): (Boolean, Boolean, Q[T[T[Sign, Std], Std]]) = {
    val r = alice * bell >>= assoc1 >>= lift1(cnot) >>= lift1(lift1(convert[Std, Sign]))
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
    (1 to 4).foreach(_ => {
      println("measure qubit: " + r1.measure().outcome)
    })

    println()
    println("** With decoherence (entangled) **")
    println("initial: " + entangled.toString)
    val r2 = entangled >>= lift1(sqrtNot)
    println("rotate 1st qubit: " + r2.toString)
    (1 to 8).foreach(_ => {
      println("measure 1st qubit: " + r2.measure(_._1).outcome)
    })

    println()
    println("Entangled qubit behaves like a classical random bit!")
  }

  def iterate[A](n: Int, a: A)(f: A => A): A = {
    if (n <= 0) a
    else iterate(n-1, f(a))(f)
  }

  /**
   * Grover's algorithm
   */
  def grover(f: Int => Int, width: Int) = {
    val Hn = liftL(H) _
    val zeroes = pure(L.fromInt(0, width))
    val one = pure(L.fromInt(1, 1))
    val inv = U(f)
    val refl = {
      val s = zeroes >>= Hn
      (s >< s) * 2 - I
    }

    val r = (math.Pi * math.sqrt(math.pow(2, width)) / 4).toInt
    // zeroes * one >>= lift12(Hn, Hn) >>= repeat(r)(inv >=> lift1(refl))
    val init = zeroes * one >>= lift12(Hn, Hn)
    iterate(r, init)(_ >>= (inv >=> lift1(refl)))
  }

  def runGrover(n: Int) = {
    def f(x: Int) = if (x == n) 1 else 0
    val bits = (math.log(n) / math.log(2)).toInt + 1
    val s = grover(f, bits)
    println("final state: " + s.toString)
    val m = L.toInt(s.measure(_._1).outcome)
    println("measurement: " + m)
  }

  // Quantum Fourier Transform
  def QFT(b: L[Std]): Q[L[Std]] = {
    def wires(theta: Double)(xs: T[Std, L[Std]]): Q[T[Std, L[Std]]] = xs match {
      case T(c, L(Nil)) => pure(T(c, L(Nil)))
      case t => {
        pure(t) >>=
        lift2(decons) >>=
        assoc1 >>=
        lift1(controlled(R(theta))) >>=
        lift1(swap) >>=
        assoc2 >>=
        lift2(wires(theta / 2)) >>=
        assoc1 >>=
        lift1(swap) >>=
        assoc2 >>=
        lift2(cons)
      }
    }
    def QFT_(b: L[Std]): Q[L[Std]] = b match {
      case L(Nil) => pure(L(Nil))
      case xs => {
        pure(xs) >>= decons >>= lift2(QFT_) >>= wires(tau / 4) >>= lift1(H) >>= cons
      }
    }
    pure(b) >>= reverse >>= QFT_
  }

  /**
   * Shor's quantum factorization algorithm (TODO)
   */
  def findPeriod(f: Int => Int, width: Int) = {

    def trial = {
      val z = pure(L.fromInt(0, width))
      val s1 = z * z >>= lift1(liftL(H)) >>= U(f) >>= lift1(QFT)
      val T(x, fx) = s1.measure().outcome
      L.toInt(x)
    }
    def gcd(a: Int, b: Int): Int = {
      if (b == 0) a
      else gcd(b, a % b)
    }
    def find = {
      val y = trial

    }

    val r = List.fill(30)(trial).reduceLeft(gcd)
    math.pow(2, width).toInt / r
  }
  def runFindPeriod = {
    def f(x: Int) = x % 4 + 1
    findPeriod(f, 5)
  }


  /**
   * Double slit experiment and Quantum Eraser
   */

  class QuantumEraser(distanceBetweenSlits: Double, distanceToScreen: Double, numDetectors: Int, distanceBetweenDetectors: Double) {

    sealed abstract class Polarization(label: String) extends Basis(label)
    case object Horizontal extends Polarization("H")
    case object Vertical extends Polarization("V")

    val h: Q[Polarization] = pure(Horizontal)
    val v: Q[Polarization] = pure(Vertical)

    val right: Q[Polarization] = (h + v*i) * rhalf
    val left: Q[Polarization] = (h - v*i) * rhalf

    val diag: Q[Polarization] = (h + v) * rhalf

    sealed abstract class Slit(label: String) extends Basis(label)
    case object A extends Slit("A")
    case object B extends Slit("B")
    
    val a: Q[Slit] = pure(A)
    val b: Q[Slit] = pure(B)
    val ab: Q[Slit] = (a + b) * rhalf

    case class Detector(n: Int) extends Basis(n.toString)

    implicit val detectorOrdering: Ordering[Detector] = Ordering.by[Detector, Int](_.n)

    val emit: Q[Polarization] = (h + v) * rhalf

    // Beta barium borate produces 2 entangled photons
    val BBO = (h⊗v >< h) + (v⊗h >< v)

    def slit[S <: Basis](s: S): Q[T[S, Slit]] = pure(s) * ab

    // Quarter wave plate turns linear polarization into circular polarization
    val qwp1 = (right >< h) + (left >< v)
    val qwp2 = (right >< v) + (left >< h)

    val QWP = (right⊗a >< h⊗a) + (left⊗a >< v⊗a) + (left⊗b >< h⊗b) + (right⊗b >< v⊗b)

    def evolve(slit: Slit): Q[Detector] = {
      val slitHeight = slit match {
        case A => distanceBetweenSlits / 2
        case B => -distanceBetweenSlits / 2
      }

      val detectors: Seq[Q[Detector]] = for (detector <- -numDetectors to numDetectors) yield {
        val height = detector * distanceBetweenDetectors - slitHeight
        val r2 = height*height + distanceToScreen*distanceToScreen
        val distance = math.sqrt(r2)
        val amplitude = (one / r2).rot(distance)
        pure(Detector(detector)) * amplitude
      }

      detectors.reduce(_ + _)
    }

    val polarizer = (diag >< diag)

    val stage1: Q[T[Polarization, T[Polarization, Detector]]] = {
      emit >>= BBO >>= lift2(slit) >>= lift2(lift2(evolve))
    }
    val stage2: Q[T[Polarization, T[Polarization, Detector]]] = {
      emit >>= BBO >>= lift2(slit) >>= lift2(QWP) >>= lift2(lift2(evolve))
    }
    val stage3: Q[T[Polarization, T[Polarization, Detector]]] = {
      emit >>= BBO >>= lift2(slit) >>= lift2(QWP) >>= lift2(lift2(evolve)) >>= lift1(polarizer)
    }
  }

  def runQuantumEraser {
    val q = new QuantumEraser(25, 100, 32, 5)
    import q._
    stage1.plotMeasurements(10000, _._2._2)
    stage2.plotMeasurements(10000, _._2._2)
    stage3.plotMeasurements(10000, _._2._2)
  }
}