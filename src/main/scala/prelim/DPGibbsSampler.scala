package prelim

import mcmc.{Multinomial, Operator, Probability}
import spire.random.Generator
import spire.std.double._

class DPGibbsSampler[A <: Double, H <: Probability[Double], X](implicit rng: Generator) extends Operator[DirichletProcess[A, H, X], Double] {

  override def apply(dp: DirichletProcess[A, H, X]): DirichletProcess[A, H, X] = dp.xs.foldLeft(dp) { (dp, x) =>
    DirichletProcess._clusters[A, H, X].modify { clusters =>
      val cp = clusters.view.map(dp.cl.remove(_, x)).filter(dp.cl.size(_) > 0)
      val logPs = cp.map(h => (h, math.log(dp.cl.size(h)) + dp.cl.posteriorPredictive(h, x))).toMap + (dp.cl.empty -> (math.log(dp.alpha) + dp.cl.posteriorPredictive(dp.cl.empty, x)))
      val scalingFactor = logPs.values.max
      val ps = logPs.mapValues(x => math.exp(x - scalingFactor))
      val hp = rng.next[H](Multinomial[H, Double](ps))
      cp.toSet - hp + dp.cl.add(hp, x)
    }(dp)
  }

  override def hastingsRatio(x: DirichletProcess[A, H, X], y: DirichletProcess[A, H, X]): Double = x.evaluate - y.evaluate

}
