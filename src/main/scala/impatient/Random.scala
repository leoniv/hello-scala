package impatient.packages

package  object random {
  private[random] var previouse: Int = 42
  private val MOD: Int = 0xFFFFFFFF

  private[random] def setSeed(seed: Int): Int = {
    previouse = math.abs(seed)
    previouse
  }

  def nextInt: Int = {
    setSeed(previouse * 1664525 + 1013904223 % MOD)
  }

  def nextDouble: Double = nextInt
}
