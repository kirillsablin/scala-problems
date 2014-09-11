package arithmetic

object P39 {
  def listPrimesInRange(range: Range):List[Int] = primes.dropWhile( _ < range.start).takeWhile( _ <= range.end).toList

  def isPrime(n:Int):Boolean = {
    val bound = math.sqrt(n).toInt
    (n > 1) && primes.takeWhile( _ <= bound).forall( n % _ != 0 )
  }

  def primes:Stream[Int] = Stream.cons(2, Stream.from(3, 2).filter( isPrime ))
}
