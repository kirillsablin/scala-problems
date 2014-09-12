package arithmetic

object P39 {
  import S99Int._
  def listPrimesInRange(range: Range):List[Int] = primes.dropWhile( _ < range.start).takeWhile( _ <= range.end).toList

}
