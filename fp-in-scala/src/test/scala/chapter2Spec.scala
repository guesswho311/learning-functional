package chapter2
import org.specs2.mutable._

class Chapter2Spec extends Specification with Chapter2 {
  "Chapter2 exercises" should {
    "be able to get fib recursively" in {
      fib(1) === 1
      fib(5) === 5
    }

    "be able to get fib without pattern matching" in {
      fib2(1, 1, 1) === 1
      fib2(5, 1, 1) === 5
    }

    "determine if an array is sorted" in {
      val sorted = Array(1,3,4,5)
      val unsorted = Array(1,5,3,4)
      def lt(a: Int, b: Int) = a <= b

      isSorted(sorted, lt _) === true
      isSorted(unsorted, lt _) === false
    }

    "determine if a list is sorted" in {
      val sorted = List(1,2,3,4,5)
      val unsorted = List(1,5,3,4)
      def lt(a: Int, b: Int) = a <= b

      isSortedList(sorted, lt _) === true
      isSortedList(unsorted, lt _) === false
    }

    "be able to curry a function" in {
      def test(a: Int, b: Double): String = (a + b).toString
      val t: Int => (Double => String) = curry(test _)
      t should beTypedEqualTo[Int => Double => String](t)
      t(1)(1) === "2.0"
    }

    "be able to uncurry a function" in {
      def test(a: Int): Double => String = (b => (a + b).toString)
      val t: (Int, Double) => String = uncurry(test _)
      t should beTypedEqualTo[(Int, Double) => String](t)
      t(1, 1.0) === "2.0"
    }

    "be able to compose two functions" in {
      def test1(a: Int): Double = a.toDouble
      def test2(b: Double): String = b.toString
      val t: Int => String = compose(test2 _, test1 _)
      t should beTypedEqualTo[Int => String](t)
      t(1) === "1.0"
    }
  }
}
