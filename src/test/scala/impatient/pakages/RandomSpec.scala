package impatient.packages.random

import org.scalatest._
import java.util.regex.Matcher

class RandomSpec extends FlatSpec with Matchers {
  nextInt should (be > 0)
  nextDouble should (be > 0.0)
  setSeed(42) should equal (42)
}
