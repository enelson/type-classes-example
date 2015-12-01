package com.enelson.typeclasses

import Math.Monoid


case class Car(name: String) {
  def add(c: Car): Car = {
    Car(name+c.name)
  }
}


object Types {

  trait Container[F[_]] {
    def foldLeft[A : Monoid](xs: F[A], f: (A,A) => A): A
  }

  object Container {

    implicit object ListContainer extends Container[List] {
      def foldLeft[A : Monoid](xs: List[A], f: (A, A) => A): A = {
        val ev = implicitly[Monoid[A]]
        xs.foldLeft(ev.zero)(f)
      }
    }

    implicit object VectorContainer extends Container[Vector] {
      def foldLeft[A : Monoid](xs: Vector[A], f: (A, A) => A): A = {
        val ev = implicitly[Monoid[A]]
        xs.foldLeft(ev.zero)(f)
      }
    }

//    implicit object MyContainerContainer extends Container[MyContainer] {
//      def foldLeft[A : Monoid](xs: MyContainer[A], f: (A, A) => A): A = {
//        val ev = implicitly[Monoid[A]]
//        xs.foldLeft(ev.zero)(f)
//      }
//    }

  }

}

object Math {

  trait Monoid[A] {
    def zero: A
    def add(x: A, y: A): A
  }

  object Monoid {

    implicit object INL extends Monoid[Int] {
      def zero: Int = 0
      def add(x: Int, y: Int): Int = x + y
    }

    implicit object DNL extends Monoid[Double] {
      def zero: Double = 0.0
      def add(x: Double, y: Double): Double = x + y
    }

    implicit object SNL extends Monoid[String] {
      def zero: String = ""
      def add(x: String, y: String): String = x + y
    }

    implicit object CNL extends Monoid[Car] {
      def zero: Car = Car("")
      def add(x: Car, y: Car): Car = x add y
    }

  }

}

object Stats3 {

  import Math._

  def sum2[A](xs: List[A])(implicit ev: Monoid[A]): A = {
    xs.foldLeft(ev.zero)(ev.add)
  }

  def sum[A : Monoid](xs: List[A]): A = {
    val ev =  implicitly[Monoid[A]]
    xs.foldLeft(ev.zero)(ev.add)
  }

}

object Main3 extends App {

  println(s"Sum: ${Stats3.sum(List(1,2,3))}")
  println(s"Sum: ${Stats3.sum(List(1.0,2,3))}")
  println(s"Sum: ${Stats3.sum(List("a","b","c"))}")
  println(s"Sum: ${Stats3.sum(List(Car("Toyota"),Car("Honda")))}")

}
