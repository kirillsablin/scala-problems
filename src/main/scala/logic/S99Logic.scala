package logic

class S99Logic(val source:Boolean) {
  def and(other:Boolean) = source && other
  def or(other: Boolean) = source || other
}

object S99Logic {
  def and(a: Boolean, b: Boolean):Boolean = a && b
  def or(a: Boolean, b: Boolean):Boolean = a || b
  def nand(a: Boolean, b: Boolean) = !(a && b)
  def notOp(a: Boolean): Boolean = !a

  implicit def toS99Logic(b: Boolean) = new S99Logic(b)

  private val booleans = List(true, false)

  def table2(f: (Boolean, Boolean) => Boolean):List[((Boolean, Boolean), Boolean)] =
    for (a <- booleans; b <- booleans) yield ((a, b), f(a, b))

  def gray(n: Int):List[String] =
    if (n == 1)
      List("0", "1")
    else {
      val prev = gray(n - 1)

      prev.map("0"+ _)  ++ prev.reverse.map("1" + _)
    }
}
