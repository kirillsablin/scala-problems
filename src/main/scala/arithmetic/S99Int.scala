package arithmetic

class S99Int(val start: Int) {
  import S99Int._
  def isPrime:Boolean = (2 to math.sqrt(start).toInt).forall( d => start % d != 0)
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}