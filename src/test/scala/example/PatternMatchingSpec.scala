package example

import org.scalatest._
import example.PatternMatching._
import org.scalatest.prop.TableDrivenPropertyChecks._

class PatternMatchingSpec extends FunSuite with DiagrammedAssertions {
  val pets = Table(
    ("name", "says", "expectKind"),
    ("Any", "meow", "cat"),
    ("Any", "nya", "cat"),
    ("Rex", "Any", "dog"),
    ("Any", "0101", "robot"),
    ("Any", "Any", "unknown"),
  )
  forAll(pets) { (name, says, expectKind) =>
    test(s"The ${Pet(name, says)} should be a $expectKind") {
      assert(kind(Pet(name, says)) == expectKind)
    }
  }
}
