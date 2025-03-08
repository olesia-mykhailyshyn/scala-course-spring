package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  include(ZeroSpecification)
  include(SuccessorSpecification)
  include(OrderingSpecification)
  include(AdditionSpecification)
  include(SubtractionSpecification)
  include(ConversionSpecification)
  include(EqualitySpecification)
  include(AdditionSubtractionSpecification)

end NumeralsSpecification

object ZeroSpecification extends Properties("Zero"):

  property("Zero should be identified as zero") = propBoolean(Zero.isZero)

  property("Zero predecessor should throw an exception") = throws(classOf[UnsupportedOperationException]):
    Zero.predecessor

  property("Zero successor should be Successor(Zero)") = propBoolean(Zero.successor == Successor(Zero))

  property("Zero should be equal to Zero") = propBoolean(Zero == Zero)

  property("Zero.toInt should return 0") = propBoolean(Zero.toInt == 0)

end ZeroSpecification

object SuccessorSpecification extends Properties("Successor"):

  property("Successor(n) should not be zero") = forAll: (n: Numeral) =>
    !Successor(n).isZero

  property("Successor(n) predecessor should be n") = forAll: (n: Numeral) =>
    Successor(n).predecessor == n

  property("Successor(n) successor should be Successor(Successor(n))") = forAll: (n: Numeral) =>
    Successor(n).successor == Successor(Successor(n))

  property("Successor(n) should not be equal to Zero") = forAll: (n: Numeral) =>
    Successor(n) != Zero

  property("Successor(n).toInt should be n.toInt + 1") = forAll: (n: Numeral) =>
    Successor(n).toInt == (n.toInt + 1)

  property("Successor(n) should be equal to n + Successor(Zero)") = forAll: (n: Numeral) =>
    Successor(n) == n + Successor(Zero)

end SuccessorSpecification

object OrderingSpecification extends Properties("Ordering"):

  property("n > Zero should be true for all Successor(n)") = forAll: (n: Numeral) =>
    n > Zero == !n.isZero

  property("Zero > n should be false for all n") = forAll: (n: Numeral) =>
    !(Zero > n)

  property("Successor(n) > n should be true") = forAll: (n: Numeral) =>
    Successor(n) > n

  property("n >= n should always be true") = forAll: (n: Numeral) =>
    n >= n

  property("n <= n should always be true") = forAll: (n: Numeral) =>
    n <= n

  property("n < Successor(n) should be true") = forAll: (n: Numeral) =>
    n < Successor(n)

  property("n > m should be equivalent to m < n") = forAll: (n: Numeral, m: Numeral) =>
    (n != m) ==> ((n > m) == (m < n))

  property("n >= m should be equivalent to m <= n") = forAll: (n: Numeral, m: Numeral) =>
    (n >= m) == (m <= n)

end OrderingSpecification

object AdditionSpecification extends Properties("Addition"):

  property("n + Zero should return n") = forAll: (n: Numeral) =>
    n + Zero == n

  property("Zero + n should return n") = forAll: (n: Numeral) =>
    Zero + n == n

  property("Successor(n) + m should be Successor(n + m)") = forAll: (n: Numeral, m: Numeral) =>
    Successor(n) + m == Successor(n + m)

  property("Addition should be commutative") = forAll: (n: Numeral, m: Numeral) =>
    n + m == m + n

  property("Addition should be associative") = forAll: (n: Numeral, m: Numeral, k: Numeral) =>
    (n + m) + k == n + (m + k)

end AdditionSpecification

object SubtractionSpecification extends Properties("Subtraction"):

  property("n - Zero should return n") = forAll: (n: Numeral) =>
    n - Zero == n

  property("Zero - n should return Zero") = forAll: (n: Numeral) =>
    Zero - n == Zero

  property("Successor(n) - Successor(m) should return n - m") = forAll: (n: Numeral, m: Numeral) =>
    (Successor(n) - Successor(m)) == (n - m)

  property("If n < m, then n - m should return Zero") = forAll: (n: Numeral, m: Numeral) =>
    if n < m then (n - m) == Zero else true

end SubtractionSpecification

object ConversionSpecification extends Properties("Conversion"):

  property("Zero.toInt should return 0") = propBoolean(Zero.toInt == 0)

  property("Successor(n).toInt should return n.toInt + 1") = forAll: (n: Numeral) =>
    Successor(n).toInt == (n.toInt + 1)

end ConversionSpecification

object EqualitySpecification extends Properties("Equality"):

  property("Zero should be equal to Zero") = propBoolean(Zero == Zero)

  property("Successor(n) should be equal to Successor(n) for the same n") = forAll: (n: Numeral) =>
    Successor(n) == Successor(n)

  property("Successor(n) should not be equal to Successor(m) if n != m") = forAll: (n: Numeral, m: Numeral) =>
    (n != m) ==> (Successor(n) != Successor(m))

  property("Zero should not be equal to any Successor(n)") = forAll: (n: Numeral) =>
    Zero != Successor(n)

  property("Equal objects should have the same hashCode") = forAll: (n: Numeral) =>
    val m = n + Zero
    (n == m) ==> (n.hashCode == m.hashCode)

end EqualitySpecification

object AdditionSubtractionSpecification extends Properties("Addition and Subtraction"):

  property("(n + m) - m should return n") = forAll: (n: Numeral, m: Numeral) =>
    (n + m) - m == n

  property("(n - m) + m should return n if n >= m") = forAll: (n: Numeral, m: Numeral) =>
    if n >= m then (n - m) + m == n else true

  property("(n + m) - (m + k) should be n - k") = forAll: (n: Numeral, m: Numeral, k: Numeral) =>
    (n + m) - (m + k) == n - k

  property("(n - m) + (m - k) should be n - k if n >= m >= k") = forAll: (n: Numeral, m: Numeral, k: Numeral) =>
    if n >= m && m >= k then (n - m) + (m - k) == n - k else true

end AdditionSubtractionSpecification
