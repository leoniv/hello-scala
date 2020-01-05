package example
import scala.annotation.tailrec

object GenericMethods {
  def tailRec[A, B](iter: A => A, comb: (B, A) => B, cond: A => Boolean)(
      seed: A,
      init: B
  ): B = {
    @tailrec
    def go(x: A, acc: B): B = {
      if (cond(x)) go(iter(x), comb(acc, x)) else acc
    }
    go(seed, init)
  }
}
