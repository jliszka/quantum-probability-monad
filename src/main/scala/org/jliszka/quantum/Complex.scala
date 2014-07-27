package org.jliszka.quantum

import scala.annotation.tailrec

case class Complex(val re: Double, val im: Double) {
  lazy val conj = Complex(re, -im)
  lazy val norm2 = re * re + im * im
  lazy val mod = math.sqrt(this.norm2)
  lazy val arg = math.atan2(im, re)

  def +(z: Complex) = Complex(re + z.re, im + z.im)
  def -(z: Complex) = Complex(re - z.re, im - z.im)
  def unary_- = this * -1.0
  def *(x: Double) = Complex(re * x, im * x)
  def *(z: Complex) = Complex(re * z.re - im * z.im, re * z.im + im * z.re)
  def /(x: Double): Complex = Complex(re / x, im / x)
  def /(z: Complex): Complex = (this * z.conj) / (z * z.conj).re

  private def polar(r: Double, t: Double) = Complex(r * math.cos(t), r * math.sin(t))
  def ^(e: Complex): Complex = {
    polar(math.pow(mod, e.re) / math.exp(arg * e.im),
	  math.log(mod) * e.im + arg * e.re)
  }
  def rot(theta: Double) = this * Complex(math.cos(theta), math.sin(theta))
  def rot90 = Complex(-im, re)
  def exp = polar(math.exp(re), im)
  def cos = (this.rot90.exp + (this.rot90 * -1).exp) / 2
  def sin = (this.rot90.exp - (this.rot90 * -1).exp) / Complex(0, 2.0)
 
  private val df = new java.text.DecimalFormat("#.#######")
  override def toString = {
    val reStr = df.format(re)
    val imStr = df.format(im) + "i"
    if (math.abs(im) < 0.00001) reStr
    else if (math.abs(re) < 0.00001) imStr
    else s"$reStr + $imStr"
  }
}

object Complex {
  def polar(mod: Double, arg: Double) = {
    new Complex(mod * math.cos(arg), mod * math.sin(arg))
  }
  implicit def toImaginary(x: Double) = new {
    def i = new Complex(0.0, x)
  }
  implicit def toImaginary(n: Int) = new {
    def i = new Complex(0.0, n.toDouble)
  }
  implicit def toComplex(x: Double) = new Complex(x, 0.0)

  val i = new Complex(0.0, 1.0)
  val one = new Complex(1.0, 0.0)
  val zero = new Complex(0.0, 0.0)

  trait ComplexIsFractional extends Fractional[Complex] with Numeric[Complex] {
    def compare(x: Complex, y: Complex) = ???
    def fromInt(x: Int) = new Complex(x, 0.0)
    def plus(x: Complex, y: Complex) = x + y
    def minus(x: Complex, y: Complex) = x - y
    def times(x: Complex, y: Complex) = x * y
    def div(x: Complex, y: Complex) = x / y
    def negate(x: Complex) = x * -1.0
    def toDouble(x: Complex) = ???
    def toFloat(x: Complex) = ???
    def toInt(x: Complex) = ???
    def toLong(x: Complex) = ???
  }

  implicit object ComplexIsFractional extends ComplexIsFractional
}

