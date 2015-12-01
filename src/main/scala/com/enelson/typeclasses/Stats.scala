package com.enelson.typeclasses

object Stats {

  def sum(xs: List[Int]): Int = {
    xs.foldLeft(0)(_ + _)
  }

}

object Main extends App {

  println(s"Sum: ${Stats.sum(List(1,2,3))}")

}
