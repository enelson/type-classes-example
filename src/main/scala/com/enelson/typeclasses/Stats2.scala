package com.enelson.typeclasses


case class Car(name: String) {
  def add(c: Car): Car = {
    Car(name+c.name)
  }
}

object Math {

  trait Monoid[A] {
    def zero: A
    def add(x: A, y: A): A
  }

  object Monoid {

    implicit object INL extends Monoid2[Int] {
      def zero: Int = 0
      def add(x: Int, y: Int): Int = x + y
    }

    implicit object DNL extends Monoid2[Double] {
      def zero: Double = 0.0
      def add(x: Double, y: Double): Double = x + y
    }

    implicit object SNL extends Monoid2[String] {
      def zero: String = ""
      def add(x: String, y: String): String = x + y
    }

    implicit object CNL extends Monoid2[Car] {
      def zero: Car = Car("")
      def add(x: Car, y: Car): Car = x add y
    }

  }

}

object Stats2 {

  import Math._

  def sum2[A](xs: List[A])(implicit ev: Monoid2[A]): A = {
    xs.foldLeft(ev.zero)(ev.add)
  }

  def sum[A : Monoid2](xs: List[A]): A = {
    val ev =  implicitly[Monoid2[A]]
    xs.foldLeft(ev.zero)(ev.add)
  }

}

object Main2 extends App {

  println(s"Sum: ${Stats2.sum(List(1,2,3))}")
  println(s"Sum: ${Stats2.sum(List(1.0,2,3))}")
  println(s"Sum: ${Stats2.sum(List("a","b","c"))}")
  println(s"Sum: ${Stats2.sum(List(Car("Toyota"),Car("Honda")))}")

}
