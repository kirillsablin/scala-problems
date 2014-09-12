package arithmetic

object P41 {
  import S99Int._

  def goldbachList(range: Range):List[(Int, Int, Int)] = range.filter( x => x > 2 && x % 2 == 0).map(x => {
    val v = x.goldbach
    (x, v._1, v._2)
  }).toList

  def goldbachListLimited(range: Range, threshold: Int) = goldbachList(range).filter( _._2 > threshold)

}
