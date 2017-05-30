package prelim.gaussian

import mcmc.Probability
import org.apache.commons.math3.distribution.NormalDistribution
import prelim.DirichletProcess.IsCluster

class GaussianCluster(val xs: Set[Double], val sigmasq: Double) extends Probability[Double] {

  val n = xs.size

  override def evaluate: Double = {
    val xsum = xs.sum
    - n / 2.0 * math.log(2 * math.Pi) - math.log(n * sigmasq + 1) / 2 - xs.view.map(x => x * x).sum / 2 + xsum * xsum * sigmasq / (2 * (n * sigmasq + 1))
  }

  def predict(x: Double): Double = {
    val xsum = xs.sum
    val `sigma_n^2` = 1.0 / (n + 1 / sigmasq)
    val `mu_n` = `sigma_n^2` * xsum
    new NormalDistribution(`mu_n`, math.sqrt(`sigma_n^2` + 1)).logDensity(x)
  }

  override def equals(that: Any): Boolean = that match {
    case that: GaussianCluster => this.xs == that.xs
    case _ => false
  }

  override def hashCode(): Int = xs.hashCode()

}

object GaussianCluster {

  def gaussianIsCluster(sigmasq: Double) = new IsCluster[GaussianCluster, Double] {

    override val empty: GaussianCluster = new GaussianCluster(Set(), sigmasq)

    override def size(h: GaussianCluster): Int = h.n

    override def of(x: Double): GaussianCluster = new GaussianCluster(Set(x), sigmasq)

    override def of(xs: Set[Double]): GaussianCluster = new GaussianCluster(xs, sigmasq)

    override def values(h: GaussianCluster): Set[Double] = h.xs

    override def add(h: GaussianCluster, x: Double): GaussianCluster = new GaussianCluster(h.xs + x, sigmasq)

    override def remove(h: GaussianCluster, x: Double): GaussianCluster = if (h.xs.contains(x))
      new GaussianCluster(h.xs - x, sigmasq)
    else
      h

    override def posteriorPredictive(h: GaussianCluster, x: Double): Double = h.predict(x)

  }

}
