package prelim

class F1[X](groundTruth: Traversable[Set[X]]) extends (Traversable[Set[X]] => Double) {

  val Pg = extractPairs(groundTruth)

  override def apply(clusters: Traversable[Set[X]]): Double = {
    val Pm = extractPairs(clusters)
    val nCommon = (Pg intersect Pm).size.toDouble
    val precision = nCommon / Pm.size
    val recall = nCommon / Pg.size
    harmonicMean(precision, recall)
  }

  def extractPairs(clusters: Traversable[Set[X]]): Set[Set[X]] = clusters.flatMap { c =>
    val cp = c.toArray
    for {
      i <- cp.indices
      j <- (i+1) until cp.length
    } yield Set(cp(i), cp(j))
  }.toSet

  def harmonicMean(x: Double, y: Double): Double = 1 / ((1/x + 1/y) / 2)

}
