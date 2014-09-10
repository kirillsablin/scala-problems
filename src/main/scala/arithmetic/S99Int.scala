package arithmetic

class S99Int(val start: Int) {
  import S99Int._
  import P32._

  def isPrime:Boolean = (2 to math.sqrt(start).toInt).forall( d => start % d != 0)
  def isCoprimeTo(other: Int):Boolean = gcd(start, other)  == 1
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}