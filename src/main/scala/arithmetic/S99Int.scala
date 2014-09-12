package arithmetic

class S99Int(val start: Int) {
  import S99Int._
  import P32._
  import lists.P10.encode

  def isPrime:Boolean = {
    val bound = math.sqrt(start).toInt
    (start > 1 ) && primes.takeWhile( _ <= bound).forall( start % _ != 0 )
  }

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

  def primeFactorMultiplicity:List[(Int, Int)] =
    encode(primeFactors) map (_.swap)

  def totientImproved:Int =
    (primeFactorMultiplicity map (x => (x._1 - 1) * math.pow(x._1, x._2 - 1).toInt)).product

  def goldbach:(Int, Int) = {
    if (start < 4 || start % 2 == 1)
      throw new IllegalArgumentException

    val first = primes.dropWhile( x => !(start - x).isPrime ).head
    (first, start - first)
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  def primes:Stream[Int] = Stream.cons(2, Stream.from(3, 2).filter( _.isPrime ))
}