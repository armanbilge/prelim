package prelim

import mcmc.Probability
import monocle.Lens
import org.apache.commons.math3.special.Gamma
import prelim.DirichletProcess.IsCluster

class DirichletProcess[A <: Double, H <: Probability[Double], X](val xs: IndexedSeq[X], val alpha: A, val clusters: Set[H])(_Z: => Double = clusters.size * math.log(alpha) - xs.indices.map(i => math.log(alpha + i)).sum)(implicit val cl: IsCluster[H, X]) extends Probability[Double] {

  lazy val Z: Double = _Z

  override lazy val evaluate: Double = Z + clusters.view.map(x => Gamma.logGamma(cl.size(x))).sum + clusters.view.map(_.evaluate).sum

}

object DirichletProcess {

  trait IsCluster[H, X] {
    def empty: H
    def size(h: H): Int
    def singleton(x: X): H
    def add(h: H, x: X): H
    def remove(h: H, x: X): H
    def posteriorPredictive(h: H, x: X): Double
  }

  implicit def _a[A <: Double, H <: Probability[Double], X]: Lens[DirichletProcess[A, H, X], A]
    = Lens[DirichletProcess[A, H, X], A](_.alpha)(alpha => dp => new DirichletProcess[A, H, X](dp.xs, alpha, dp.clusters)(dp.Z)(dp.cl))

  implicit def _clusters[A <: Double, H <: Probability[Double], X]: Lens[DirichletProcess[A, H, X], Set[H]]
    = Lens[DirichletProcess[A, H, X], Set[H]](_.clusters)(clusters => dp => new DirichletProcess[A, H, X](dp.xs, dp.alpha, clusters)()(dp.cl))

}
