package kse.unit2.challenge

import kse.model.DummyError
import kse.unit2.challenge.booleans.*
import kse.unit2.challenge.generators.given
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object BooleansSpecification extends Properties("Booleans"):

  include(NegationSpecification)
  include(ConjunctionSpecification)
  include(DisjunctionSpecification)
  include(ImplicationSpecification)
  include(EquivalenceSpecification)
  include(AxiomsSpecification)
  include(FoldSpecification)

end BooleansSpecification

object NegationSpecification extends Properties("Negation"):

  property("!True is False") = propBoolean:
    unary_!(True) == False

  property("!False is True") = propBoolean:
    unary_!(False) == True

end NegationSpecification

object ConjunctionSpecification extends Properties("Conjunction"):

  property("True ∧ value is value") = forAll: (value: Boolean) =>
    True ∧ value == value

  property("False ∧ value is False") = forAll: (value: Boolean) =>
    False ∧ value == False

  property("False ∧ value should ignore the second argument") = forAll: (value: Boolean) =>
    (False ∧ (throw DummyError("Should not be thrown"))) == False

end ConjunctionSpecification

object DisjunctionSpecification extends Properties("Disjunction"):

  property("True ∨ value is True") = forAll: (value: Boolean) =>
    True ∨ value == True

  property("True ∨ value should ignore the second argument") = forAll: (value: Boolean) =>
    (True ∨ (throw DummyError("Should not be thrown"))) == True

  property("False ∨ value is value") = forAll: (value: Boolean) =>
    False ∨ value == value

end DisjunctionSpecification

object ImplicationSpecification extends Properties("Implication"):

  property("True → value is value") = forAll: (value: Boolean) =>
    True → value == value

  property("False → value is True") = forAll: (value: Boolean) =>
    False → value == True

  property("False → value should ignore the second argument") = forAll: (value: Boolean) =>
    (False → (throw DummyError("Should not be thrown"))) == True

end ImplicationSpecification

// Optional challenge
object EquivalenceSpecification extends Properties("Equivalence"):

  property("True ↔ True is True") = forAll: (value: Boolean) =>
    True ↔ True == True

  property("False ↔ False is True") = forAll: (value: Boolean) =>
    False ↔ False == True

  property("True ↔ False is False") = forAll: (value: Boolean) =>
    True ↔ False == False

  property("False ↔ True is False") = forAll: (value: Boolean) =>
    False ↔ True == False

end EquivalenceSpecification

object AxiomsSpecification extends Properties("Axioms"):

  property("a → (b → a)") = forAll: (a: Boolean, b: Boolean) =>
    a → (b → a) == True

  property("(a → (b → c)) → ((a → b) → (a → c))") = forAll: (a: Boolean, b: Boolean, c: Boolean) =>
    (a → (b → c)) → ((a → b) → (a → c)) == True

  property("(a ∧ b) → a") = forAll: (a: Boolean, b: Boolean) =>
    (a ∧ b) → a == True

  property("(a ∧ b) → b") = forAll: (a: Boolean, b: Boolean) =>
    (a ∧ b) → b == True

  property("a → (b → (a ∧ b))") = forAll: (a: Boolean, b: Boolean) =>
    a → (b → (a ∧ b)) == True

  property("a → (a ∨ b)") = forAll: (a: Boolean, b: Boolean) =>
    a → (a ∨ b) == True

  property("b → (a ∨ b)") = forAll: (a: Boolean, b: Boolean) =>
    b → (a ∨ b) == True

  property("(a → c) → ((b → c) → ((a ∨ b) → c))") = forAll: (a: Boolean, b: Boolean, c: Boolean) =>
    (a → c) → ((b → c) → ((a ∨ b) → c)) == True

  property("!a → (a → b)") = forAll: (a: Boolean, b: Boolean) =>
    !a → (a → b) == True

  property("(a → b) → ((a → !b) → !a)") = forAll: (a: Boolean, b: Boolean) =>
    (a → b) → ((a → !b) → !a) == True

  property("a ∨ !a") = forAll: (a: Boolean) =>
    a ∨ !a == True

end AxiomsSpecification

object FoldSpecification extends Properties("Fold"):

  property("Conjunction of all elements from the empty list should be True") = propBoolean:
    Nil.conjunction == True

  property("Conjunction of all elements from non-empty list should be correctly evaluated") = forAll: (booleans: List[Boolean]) =>
    booleans.conjunction == booleans.foldRight[Boolean](True)((value, acc) => value ∧ acc)

  property("Disjunction of all elements from the empty list should be False") = propBoolean:
    Nil.disjunction == False

  property("Disjunction of all elements from non-empty list should be correctly evaluated") = forAll: (booleans: List[Boolean]) =>
    booleans.disjunction == booleans.foldRight[Boolean](False)((value, acc) => value ∨ acc)

end FoldSpecification
