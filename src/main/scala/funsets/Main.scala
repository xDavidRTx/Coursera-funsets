package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  singletonSet(2)(2)
}
