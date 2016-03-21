package tasks

object StreamProblems {}

object P01 extends App {

  def from(n: Int): Stream[Int] = n #:: from(n +1)

  val first = from(10)
  val firstRes = first map (_ * 4)
  println(firstRes)
  println(firstRes.take(10).toList)

  def sierv(s: Stream[Int]):Stream[Int] = s.head #:: sierv(s.tail filter (_ % s.head != 0))

  val prime = sierv(from(2))
  println(prime.take(10).toList)

}
