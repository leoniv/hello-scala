package example

object Hello extends Greeting with App {
  println(greeting("world"))
}

trait Greeting {
  def greeting(who: String): String = s"hello $who"
}
