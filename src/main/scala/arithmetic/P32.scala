package arithmetic

object P32 {
  def gcd(a: Int, b: Int):Int = {
    val r = a % b
    if (r == 0)
      b
    else
      gcd(b, r)
  }
}
