package prelim

import mcmc.{Operator, Probability}
import spire.random.Generator

class ProcesserSampler[A <: Double, H <: Probability[Double], X](implicit rng: Generator) extends Operator[IndexedSeq[DirichletProcess[A, H, X]], Double] {

  override def apply(dps: IndexedSeq[DirichletProcess[A, H, X]]): IndexedSeq[DirichletProcess[A, H, X]] = {
    val k = rng.chooseFromIterable(dps.flatMap(_.clusters))
    val j = rng.nextInt(dps.length)
    dps.view.map(dp => if (dp.clusters.contains(k)) DirichletProcess._clusters[A, H, X].modify(_ - k)(dp) else dp)
      .zipWithIndex.map(Function.tupled((dp, i) => if (i == j) DirichletProcess._clusters[A, H, X].modify(_ + k)(dp) else dp))
      .toIndexedSeq
  }

  override def hastingsRatio(x: IndexedSeq[DirichletProcess[A, H, X]], y: IndexedSeq[DirichletProcess[A, H, X]]): Double = 0.0

}
