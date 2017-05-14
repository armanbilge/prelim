package prelim

import mcmc.Probability
import monocle.Lens
import org.apache.commons.math3.special.Gamma
<<<<<<< HEAD
import prelim.DirichletProcess.Cluster

class DirichletProcess[A <: Double, H <: Probability[Double], X](val xs: IndexedSeq[X], val alpha: A, val clusters: Set[H])(_Z: => Double = clusters.size * math.log(alpha) - xs.indices.map(i => math.log(alpha + i)).sum)(implicit val cl: Cluster[H, X]) extends Probability[Double] {

  lazy val Z: Double = _Z

  override lazy val evaluate: Double = Z + clusters.view.map(x => Gamma.logGamma(cl.size(x))).sum + clusters.view.map(_.evaluate).sum
=======
import prelim.DirichletProcess.IsCluster

class DirichletProcess[A <: Double, H <: Probability[Double], X](_xs: => IndexedSeq[X], val alpha: A, val clusters: Set[H])(implicit val cl: IsCluster[H, X]) extends Probability[Double] {

  lazy val xs: IndexedSeq[X] = _xs

  lazy val Z: Double = clusters.size * math.log(alpha) - xs.indices.map(i => math.log(alpha + i)).sum

  override lazy val evaluate: Double =  Z + clusters.view.map(x => Gamma.logGamma(cl.size(x))).sum + clusters.view.map(_.evaluate).sum

  override def toString: String = clusters.mkString("{", ",", "}")
>>>>>>> 55f42e4b3c7b05c645531c299d9d1d1c5fae32a4

}

object DirichletProcess {

<<<<<<< HEAD
  trait Cluster[H, X] {
    def empty: H
    def size(h: H): Int
    def singleton(x: X): H
    def add(h: H, x: X): H
=======
  def apply[A <: Double, H <: Probability[Double], X](xs: IndexedSeq[X], alpha: A)(implicit cl: IsCluster[H, X]) = new DirichletProcess[A, H, X](xs, alpha, xs.map(cl.of).toSet)

  trait IsCluster[H, X] {
    def empty: H
    def size(h: H): Int
    def of(x: X): H
    def of(xs: Set[X]): H
    def add(h: H, x: X): H
    def values(h: H): Set[X]
>>>>>>> 55f42e4b3c7b05c645531c299d9d1d1c5fae32a4
    def remove(h: H, x: X): H
    def posteriorPredictive(h: H, x: X): Double
  }

  implicit def _a[A <: Double, H <: Probability[Double], X]: Lens[DirichletProcess[A, H, X], A]
<<<<<<< HEAD
    = Lens[DirichletProcess[A, H, X], A](_.alpha)(alpha => dp => new DirichletProcess[A, H, X](dp.xs, alpha, dp.clusters)(dp.Z)(dp.cl))

  implicit def _clusters[A <: Double, H <: Probability[Double], X]: Lens[DirichletProcess[A, H, X], Set[H]]
    = Lens[DirichletProcess[A, H, X], Set[H]](_.clusters)(clusters => dp => new DirichletProcess[A, H, X](dp.xs, dp.alpha, clusters)()(dp.cl))
=======
    = Lens[DirichletProcess[A, H, X], A](_.alpha)(alpha => dp => new DirichletProcess[A, H, X](dp.xs, alpha, dp.clusters)(dp.cl))

  implicit def _clusters[A <: Double, H <: Probability[Double], X]: Lens[DirichletProcess[A, H, X], Set[H]]
    = Lens[DirichletProcess[A, H, X], Set[H]](_.clusters)(clusters => dp => new DirichletProcess[A, H, X](clusters.view.flatMap(dp.cl.values).toIndexedSeq, dp.alpha, clusters)(dp.cl))
>>>>>>> 55f42e4b3c7b05c645531c299d9d1d1c5fae32a4

}
