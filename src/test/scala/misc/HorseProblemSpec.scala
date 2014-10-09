package misc

import org.scalatest._

class HorseProblemSpec extends FlatSpec with Matchers {
  case class Point(x:Int, y:Int)

  def findPath(start: Point, end: Point):List[Point] = {
    def getNewPoints(from:Point):List[Point] = List(
      Point(from.x-1, from.y-2),
      Point(from.x-1, from.y+2),
      Point(from.x+1, from.y-2),
      Point(from.x+1, from.y+2),
      Point(from.x-2, from.y-1),
      Point(from.x-2, from.y+1),
      Point(from.x+2, from.y-1),
      Point(from.x+2, from.y+1)
    )
    def findPath(visited:Set[Point], frontier:List[(Point, List[Point])]):List[Point] =
      if (frontier.head._1 == end)
        frontier.head._2
      else
        findPath(
          visited + frontier.head._1,
          frontier.tail ++ getNewPoints(frontier.head._1).filter(!visited.contains(_)).map( n => (n , n::frontier.head._2))
        )

    findPath(Set(), List((start, List(start)))).reverse
  }

  "findPath" should "return path consists of one point if start point is same as end point" in {
    findPath(Point(1, 1), Point(1, 1)) should be (List(Point(1, 1)))
  }

  it should "return shortest path between points" in {
    findPath(Point(1, 1), Point(2, 3)) should be (List(Point(1, 1), Point(2, 3)))
    findPath(Point(1, 1), Point(3, 5)) should be (List(Point(1, 1), Point(2, 3), Point(3, 5)))
  }

}
