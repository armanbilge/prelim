package prelim

import mcmc.{Operator, Probability}
import spire.random.Generator

class SimpleDPOperator[A <: Double, H <: Probability[Double], X](implicit rng: Generator) extends Operator[DirichletProcess[A, H, X], Double] {

  override def apply(dp: DirichletProcess[A, H, X]): DirichletProcess[A, H, X] = {
    val x = dp.xs(rng.nextInt(dp.xs.size))
    DirichletProcess._clusters[A, H, X].modify { clusters =>
      val cp = clusters.map(dp.cl.remove(_, x)) + dp.cl.empty
      val i = rng.nextInt(cp.size)
      cp.view.zipWithIndex.map(Function.tupled((h, j) => if (i == j) dp.cl.add(h, x) else h)).filter(dp.cl.size(_) > 0).toSet
    }(dp)
  }

  override def hastingsRatio(x: DirichletProcess[A, H, X], y: DirichletProcess[A, H, X]): Double = 0

}
