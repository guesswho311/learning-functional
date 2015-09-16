package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => t
  }

  def init[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def drop[A](ls: List[A], n: Int): List[A] = n match {
    case 0 => ls
    case n => drop(tail(ls), n - 1)
  }

  def dropWhile[A](ls: List[A])(f: A => Boolean): List[A] = ls match {
    case Nil => Nil
    case Cons(h, t) if (f(h)) => dropWhile(t)(f)
    case _ => ls
  }

  def setHead[A](head: A, ls: List[A]) = ls match {
    case Nil => Cons(head, Nil)
    case Cons(h,t) => Cons(head, t)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], acc: B)(f: (A, B) => B): B = as match{
    case Nil => acc
    case Cons(h, t) => f(h, foldRight(t, acc)(f))
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], acc:B)(f: (B,A) => B): B = as match {
    case Nil => acc
    case Cons(h,t) => foldLeft(t, f(acc, h))(f)
  }

  def sum2(ls: List[Int]): Int =
    foldLeft(ls, 0)(_ + _)

  def product2(ls: List[Double]): Double =
    foldLeft(ls, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((b, a) => b + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  //TODO: 3.13
  //def foldLeftUsingRight[A, B](ls: List[A], acc: B): B =
  //  foldRight(ls, acc)(())

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((b, a) => Cons(a, b))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def flatten[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])((a, b) => append(a, b))

  def flatten2[A](ls: List[List[A]]): List[A] = ls match {
    case Nil => Nil
    case Cons(h, t) => append(h, flatten2(t))
  }

  def add1ToEach(ls: List[Int]): List[Int] = {
    def add1ToEachRec(current: List[Int], result: List[Int]): List[Int] = current match {
      case Nil => result
      case Cons(h, t) => add1ToEachRec(t, append(result, List(h + 1)))
    }

    add1ToEachRec(ls, Nil)
  }

  def elementsToString[A](ls: List[A]): List[String] =
    foldRight(ls, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    def mapR(as: List[A], result: List[B]): List[B] = as match {
      case Nil => reverse(result)
      case Cons(h, t) => mapR(t, Cons(f(h), result))
    }
    mapR(as, Nil)
  } // gonna be able to do this waaay better with monads

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def filterR(current: List[A], result: List[A]): List[A] = current match{
      case Nil => result
      case Cons(h, t) if (f(h)) => filterR(t, append(result, List(h)))
      case Cons(h, t) => filterR(t, result)
    }
    filterR(as, Nil)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def filterViaFlatmap[A](ls: List[A])(f: A => Boolean) =
    flatMap(ls)(i => if (f(i)) List(i) else Nil)

  def addPairs(ls: List[Int], ls2: List[Int]): List[Int] = (ls, ls2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(h1 + h2, addPairs(t1, t2))
  }

  def zipWith[A,B,C](ls: List[A], ls2: List[B])(f: (A,B) => C): List[C] = (ls, ls2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
}
