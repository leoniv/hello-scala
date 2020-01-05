package example

import org.scalatest._
import example.GenericMethods._

class GenericMethodsSpec extends FlatSpec with Matchers {
  "The tailRec method" should "perfectly works" in {
    tailRec[Int, Int](_ + 1, _ + _, _ <= 5)(1, 0) shouldEqual 15
  }
}
