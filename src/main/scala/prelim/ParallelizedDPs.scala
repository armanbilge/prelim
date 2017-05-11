package prelim

import mcmc.Probability
import monocle.Lens
import org.apache.commons.math3.special.Gamma

class ParallelizedDPs[A <: Double, H <: Probability[Double], X](val alpha: A, val dps: IndexedSeq[DirichletProcess[A, H, X]])(val n: Int = dps.map(_.clusters.size).sum, val K: Int = dps.length, val a: Double = alpha / dps.length)(_Z: => Double = - Gamma.logGamma(n + 1) - Gamma.logGamma(alpha) + K * Gamma.logGamma(a) + Gamma.logGamma(n + alpha)) extends Probability[Double] {

  lazy val Z = _Z

  lazy val evaluate: Double = - Z + dps.map(dp => - Gamma.logGamma(dp.clusters.size + 1) + Gamma.logGamma(dp.clusters.size + a) + dp.evaluate).sum

}

object ParallelizedDPs {

  def apply[A <: Double, H <: Probability[Double], X](alpha: A, dps: IndexedSeq[DirichletProcess[A, H, X]]) =
    new ParallelizedDPs[A, H, X](alpha, dps)()()

  implicit def _alpha[A <: Double, H <: Probability[Double], X]: Lens[ParallelizedDPs[A, H, X], A] =
    Lens[ParallelizedDPs[A, H, X], A](_.alpha)(alpha => pdp => ParallelizedDPs[A, H, X](alpha, pdp.dps.map(DirichletProcess._a[A, H, X].set((alpha / pdp.K).asInstanceOf[A]))))

  implicit def _dps[A <: Double, H <: Probability[Double], X]: Lens[ParallelizedDPs[A, H, X], IndexedSeq[DirichletProcess[A, H, X]]] =
    Lens[ParallelizedDPs[A, H, X], IndexedSeq[DirichletProcess[A, H, X]]](_.dps)(dps => pdp => new ParallelizedDPs[A, H, X](pdp.alpha, dps)(pdp.n, pdp.K, pdp.a)(pdp.Z))

}
