package prelim

import mcmc.Multinomial
import spire.random.{Dist, Generator}
import spire.std.double._

import scala.collection.mutable

object DirichletProcessSimulator {

  def apply[X](alpha: Double, h: Dist[X])(implicit rng: Generator): TraversableOnce[X] = new Iterator[X]() {

      var n = 0
      val clusters = mutable.Map[X, Double]()

      override val hasNext: Boolean = true

      override def next(): X = {

        val draw = if (rng.nextDouble() < alpha / (alpha + n)) {
          val x = rng.next(h)
          clusters(x) = 1
          x
        } else {
          val x = rng.next(Multinomial[X, Double](clusters.toMap))
          clusters(x) = clusters(x) + 1
          x
        }

        n += 1
        draw

      }

    }

}
