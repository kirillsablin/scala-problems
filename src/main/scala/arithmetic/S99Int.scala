package arithmetic

class S99Int(val start: Int) {
  import S99Int._
  import P32._

  def isPrime:Boolean = (2 to math.sqrt(start).toInt).forall( d => start % d != 0)

  def isCoprimeTo(other: Int):Boolean = gcd(start, other)  == 1

  def totient:Int = (1 to start - 1) count (_.isCoprimeTo(start))

  def primeFactors:List[Int] =
    if (start == 1)
      List()
    else {
      for (r <- 2 to math.sqrt(start).toInt + 1 if start % r == 0) {
        return r :: (start / r).primeFactors
      }
      List(start)
    }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}