package prelim

import mcmc.{Multinomial, Operator, Probability}
import spire.random.Generator
import spire.std.double._

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

class DPGibbsSampler[A <: Double, H <: Probability[Double], X](shuffle: Boolean = true)(implicit rng: Generator) extends Operator[DirichletProcess[A, H, X], Double] {

  override def apply(dp: DirichletProcess[A, H, X]): DirichletProcess[A, H, X] = {
    val scan = if (shuffle)
      shuffle(dp.xs)
    else
      dp.xs
    scan.foldLeft(dp) { (dp, x) =>
      DirichletProcess._clusters[A, H, X].modify { clusters =>
        val cp = clusters.view.map(dp.cl.remove(_, x)).filter(dp.cl.size(_) > 0)
        val logPs = cp.map(h => (h, math.log(dp.cl.size(h)) + dp.cl.posteriorPredictive(h, x))).toMap + (dp.cl.empty -> (math.log(dp.alpha) + dp.cl.posteriorPredictive(dp.cl.empty, x)))
        val scalingFactor = logPs.values.max
        val ps = logPs.mapValues(x => math.exp(x - scalingFactor))
        val hp = rng.next[H](Multinomial[H, Double](ps))
        cp.toSet - hp + dp.cl.add(hp, x)
      }(dp)
    }
  }

  override def hastingsRatio(x: DirichletProcess[A, H, X], y: DirichletProcess[A, H, X]): Double = x.evaluate - y.evaluate

  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = rng.nextInt(n)
      swap(n - 1, k)
    }

    (bf(xs) ++= buf).result()
  }

}
