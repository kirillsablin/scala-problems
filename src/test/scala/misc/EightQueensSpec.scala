package misc

import org.scalatest._

class EightQueensSpec extends FlatSpec with Matchers {

  import EightQueens._

  "isValidPosition" should "return true if no previous queens interacts with new one" in {
    isValidPosition(List(), 1, 1) should be (true)
    isValidPosition(List(Queen(1, 1)), 2, 3) should be (true)
    isValidPosition(List(Queen(1, 1), Queen(2, 3)), 3, 5) should be (true)
  }

  it should "return false if any of previous queens shares same row with new one" in {
    isValidPosition(List(Queen(1, 1)), 2, 1) should be (false)
    isValidPosition(List(Queen(1, 1), Queen(2, 8)), 3, 8) should be (false)
  }

  it should "return false if any of previous queens shares same diagonal with new one" in {
    isValidPosition(List(Queen(1, 1), Queen(2, 7)), 3, 3) should be (false)
  }


  "findSolution" should "return valid solution" in {
    val solution = findSolution

    val (_, valid) = solution.foldLeft((List[Queen](), true))( (prevResult, currentElement) =>
      (currentElement::prevResult._1, prevResult._2 && isValidPosition(prevResult._1, currentElement.x, currentElement.y)))

    valid should be (true)
  }


}
