/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package lingo.corefp.utils

import java.math.BigInteger

import scala.math.{BigInt, Ordered, ScalaNumber, ScalaNumericConversions}

final object Nat {
  def bigInt2nat(x: BigInt) = new Nat(x)

  def apply(x: BigInt) = bigInt2nat(x)

  def apply(i: Int) = bigInt2nat(BigInt.apply(i))

  def apply(l: Long) = bigInt2nat(BigInt.apply(l))

  def apply(x: Array[Byte]) = bigInt2nat(BigInt.apply(x))

  def apply(signum: Int, magnitude: Array[Byte]) = bigInt2nat(BigInt.apply(signum, magnitude))

  def apply(bitlength: Int, certainty: Int, rnd: scala.util.Random) = bigInt2nat(BigInt.apply(bitlength, certainty, rnd))

  def apply(numbits: Int, rnd: scala.util.Random) = bigInt2nat(BigInt.apply(numbits, rnd))

  def apply(x: String) = bigInt2nat(BigInt.apply(x))

  def apply(x: String, radix: Int) = bigInt2nat(BigInt.apply(x, radix))

  def apply(x: BigInteger) = bigInt2nat(BigInt.apply(x))

  def probablePrime(bitLength: Int, rnd: scala.util.Random) = bigInt2nat(BigInt.probablePrime(bitLength, rnd))

  val One = Nat(1)
  val Zero = Nat(0)
}

final class Nat(val bigInt: BigInt)
  extends ScalaNumber
    with ScalaNumericConversions
    with Serializable
    with Ordered[Nat] {
  if (bigInt < 0) {
    throw new IllegalArgumentException("expect a postive number")
  }

  override def hashCode(): Int = bigInt.hashCode()

  override def equals(that: Any): Boolean = that equals bigInt

  override def isValidByte = bigInt.isValidByte

  override def isValidShort = bigInt.isValidShort

  override def isValidChar = bigInt.isValidChar

  override def isValidInt = bigInt.isValidInt

  def isValidLong: Boolean = bigInt.isValidLong

  def isValidFloat: Boolean = bigInt.isValidFloat

  def isValidDouble: Boolean = bigInt.isValidDouble

  def isWhole: Boolean = bigInt.isWhole

  def underlying: BigInt = bigInt

  def equals(that: Nat): Boolean = bigInt.equals(that.bigInt)

  def compare(that: Nat): Int = bigInt.compare(that.bigInt)

  def +(that: Nat) = new Nat(bigInt + that.bigInt)

  def -(that: Nat) = new Nat(bigInt - that.bigInt)

  def *(that: Nat) = new Nat(bigInt * that.bigInt)

  def /(that: Nat) = new Nat(bigInt / that.bigInt)

  def %(that: Nat) = new Nat(bigInt % that.bigInt)

  def /%(that: Nat): (Nat, Nat) = {
    val (x, y) = bigInt /% that.bigInt
    (new Nat(x), new Nat(y))
  }

  def <<(n: Int) = new Nat(bigInt << n)

  def >>(n: Int) = new Nat(bigInt >> n)

  def &(that: Nat) = new Nat(bigInt & that.bigInt)

  def |(that: Nat) = new Nat(bigInt | that.bigInt)

  def ^(that: Nat) = new Nat(bigInt ^ that.bigInt)

  def &~(that: Nat) = new Nat(bigInt &~ that.bigInt)

  def gcd(that: Nat) = new Nat(bigInt.gcd(that.bigInt))

  def mod(that: Nat) = new Nat(bigInt.mod(that.bigInt))

  def min(that: Nat) = new Nat(bigInt.min(that.bigInt))

  def max(that: Nat) = new Nat(bigInt.max(that.bigInt))

  def pow(exp: Int) = new Nat(bigInt.pow(exp))

  def modPow(exp: Nat, m: Nat) = new Nat(bigInt.modPow(exp.bigInt, m.bigInt))

  def modInverse(m: Nat) = new Nat(bigInt.modInverse(m.bigInt))

  def unary_- = new Nat(bigInt.unary_-)

  def abs: Nat = this

  def signum: Int = bigInt.signum

  def sign: BigInt = bigInt.sign

  def unary_~ = new Nat(bigInt.unary_~)

  def testBit(n: Int): Boolean = bigInt.testBit(n)

  def setBit(n: Int) = new Nat(bigInt.setBit(n))

  def clearBit(n: Int) = new Nat(bigInt.clearBit(n))

  def flipBit(n: Int) = new Nat(bigInt.flipBit(n))

  def lowestSetBit: Int = bigInt.lowestSetBit

  def bitLength = bigInt.bitLength

  def isProbablePrime(certainty: Int) = bigInt.isProbablePrime(certainty)

  override def byteValue: Byte = bigInt.byteValue

  override def shortValue: Short = bigInt.shortValue

  def charValue: Char = bigInt.charValue

  def intValue: Int = bigInt.intValue

  def longValue: Long = bigInt.longValue

  def floatValue: Float = bigInt.floatValue

  def doubleValue: Double = bigInt.doubleValue

  // TODO: until, to

  override def toString() = bigInt.toString()

  def toString(radix: Int) = bigInt.toString(radix)

  def toByteArray: Array[Byte] = bigInt.toByteArray

  def succ: Nat = this + Nat.One
}
