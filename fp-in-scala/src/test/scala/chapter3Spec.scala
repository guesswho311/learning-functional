package chapter3
import org.specs2.mutable._

class Chapter3Spec extends Specification {
  import List._

  "My List" should {
    val ls = List(1,2,3)
    val ls2 = List(4,5,6)
    "be able to find tail" in {
      tail(ls) === Cons(2, Cons(3, Nil))
    }

    "be able to find all but last" in {
      init(ls) === Cons(1, Cons(2, Nil))
    }

    "drop n items" in {
      drop(ls, 2) === Cons(3, Nil)
    }

    "drop while a condition is true" in {
      dropWhile(ls)((a: Int) => a < 2) === Cons(2, Cons(3, Nil))
    }

    "set head item" in {
      setHead(5, ls) === Cons(5,Cons(2,Cons(3, Nil)))
    }

    "sum a list" in {
      sum(ls) === 6
      sum2(ls) === 6
    }

    "find product of list" in {
      product(List(1.0, 2.0, 3.0)) === 6.0
      product2(List(1.0, 2.0, 3.0)) === 6.0
    }

    "construct a list" in {
      apply(1,2,3) === Cons(1, Cons(2, Cons(3, Nil)))
    }

    "foldRight" in {
      foldRight(ls, 1)(_ * _) === 6.0
    }

    "find length of list" in {
      List.length(ls) === 3
      List.length2(ls) === 3
    }

    "fold left" in {
      foldLeft(ls, 1)(_ * _) === 6.0
    }

    "reverse a list" in {
      reverse(ls) === List(3,2,1)
    }

    "append two lists together" in {
      append(ls, ls2) === List(1,2,3,4,5,6)
      append2(ls, ls2) === List(1,2,3,4,5,6)
      append3(ls, ls2) === List(1,2,3,4,5,6)
    }

    "flatten a nested list" in {
      val nestedList = List(List(1,2,3), List(4,5,6))
      flatten(nestedList) === List(1,2,3,4,5,6)
      flatten2(nestedList) === List(1,2,3,4,5,6)
    }

    "add 1 to each element of a list of ints" in {
      add1ToEach(ls) === List(2,3,4)
    }

    "turn each element to a string" in {
      elementsToString(ls) === List("1", "2", "3")
    }

    "map over a list" in {
      List.map(ls)((a: Int) => a + 1) === List(2,3,4)
    }

    "filter a list" in {
      List.filter(ls)((a: Int) => a % 2 == 0) === List(2)
    }

    "flatmap a list" in {
       flatMap(ls)(i => List(i,i)) === List(1,1,2,2,3,3)
    }

    "filter using flatmap" in {
      filterViaFlatmap(ls)((a: Int) => a % 2 == 0) === List(2)
    }

    "add pairs" in {
      addPairs(ls, ls2) === List(5, 7, 9)
    }

    "zip two lists by a function" in {
      zipWith(ls, ls2)((a,b) => a * b) === List(4, 10, 18)
    }
  }

}
