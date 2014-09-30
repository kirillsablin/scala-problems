package mtree

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def lispyTree:String = if (children.isEmpty) value.toString else children.foldLeft("(" + value)(_ + " " + _.lispyTree) + ")"

  def postorder:List[T] = children.flatMap( _.postorder) ::: List(value)

  def nodeCount:Int = children.foldLeft(1)( _ + _.nodeCount )

  def this(value: T) = this(value, List())
  override def toString = value + children.foldLeft("")(_ + _) + "^"

  def internalPathLengthWithStart(start:Int):Int = start + children.map( _.internalPathLengthWithStart(start + 1)).sum
  def internalPathLength:Int = internalPathLengthWithStart(0)
}

object MTree {
  def fromLispy(s: String):MTree[String] = {
    def readIdentifier(from:Int):(String, Int) = {
      val identifier = s.drop(from).takeWhile(_.isLetter)
      (identifier, from + identifier.length)
    }

    def nextNonSpace(from:Int):Int = if (s(from) == ' ') nextNonSpace(from + 1) else from

    def parseChildren(from:Int):(List[MTree[String]], Int) = {
      if (s(from) == ')') {
        (List(), from + 1)
      }
      else {
        val (current, after) = parse(from)
        val (rest, afterChildren) = parseChildren(nextNonSpace(after))
        (current::rest, afterChildren)
      }
    }

    def parse(from:Int):(MTree[String], Int) = s(from) match {
      case a if a.isLetter =>
        val (identifier, end) = readIdentifier(from)
        (MTree(identifier), end)
      case '(' =>
        val (value, further) = readIdentifier(nextNonSpace(from + 1))
        val (children, afterNode) = parseChildren(nextNonSpace(further))
        (MTree(value, children), afterNode)
    }

    parse(nextNonSpace(0))._1
  }

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
