package mtree

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def nodeCount:Int = children.foldLeft(1)( _ + _.nodeCount )

  def this(value: T) = this(value, List())
  override def toString = value + children.foldLeft("")(_ + _) + "^"
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())
  implicit def fromString(str:String):MTree[Char] = {
    def parseSequence(s:List[Char], soFar:List[MTree[Char]] = List()):(List[MTree[Char]], List[Char]) = s match {
      case '^' :: rest => (soFar, rest)
      case _ =>
        val (child, rest) = parse(s)
        parseSequence(rest, soFar :+ child)
    }
    def parse(s: List[Char]):(MTree[Char], List[Char]) = s match {
      case a::'^'::rest if a.isLetter => (MTree(a), rest)
      case a::rest =>
        val (children, parentRest) = parseSequence(rest)

        (MTree(a, children), parentRest)
    }

    parse(str.toList)._1
  }
}
