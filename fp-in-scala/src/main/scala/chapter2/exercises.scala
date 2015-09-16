package chapter2

trait Chapter2 {
  def fib(n: Int): Int = {
    def fibRec(n: Int, prev: Int, cur: Int): Int = n match {
      case a if (a <= 1) => prev
      case a => fibRec(n - 1, cur, prev + cur)
    }
    fibRec(n, 1, 1)
  }

  def fib2(n: Int, prev: Int, cur: Int): Int = {
    if (n <= 1) prev
    else fib2(n - 1, cur, prev + cur)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def check(pos: Int): Boolean = {
      if (pos + 1 == as.length)
        ordered(as(pos - 1), as(pos))
      else if (!(ordered(as(pos), as(pos + 1))))
        false
      else check(pos + 1)

    }
    check(0)
  }

  def isSortedList[A](ls: List[A], ordered: (A, A) => Boolean): Boolean = ls match {
    case a :: b :: Nil => ordered(a,b)
    case a :: b :: tail if(ordered(a,b)) => isSortedList(b :: tail, ordered)
    case a :: b :: tail => false
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
