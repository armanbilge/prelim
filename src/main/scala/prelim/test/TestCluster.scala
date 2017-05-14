package prelim.test

import mcmc.Probability
import prelim.DirichletProcess.IsCluster

class TestCluster[X](val xs: Set[X]) extends Probability[Double] {

  val evaluate: Double = 0

  override def equals(that: Any): Boolean = that match {
    case that: TestCluster[X] => this.xs == that.xs
    case _ => false
  }

  override def hashCode(): Int = xs.hashCode()

  override def toString: String = xs.mkString("{", ",", "}")

}

object TestCluster {

  implicit def testIsCluster[X]: IsCluster[TestCluster[X], X] = new IsCluster[TestCluster[X], X] {

    override def empty: TestCluster[X] = new TestCluster(Set.empty[X])

    override def size(h: TestCluster[X]): Int = h.xs.size

    override def of(x: X): TestCluster[X] = new TestCluster(Set(x))

    override def of(xs: Set[X]): TestCluster[X] = new TestCluster(xs)

    override def add(h: TestCluster[X], x: X): TestCluster[X] = new TestCluster(h.xs + x)

    override def values(h: TestCluster[X]): Set[X] = h.xs

    override def remove(h: TestCluster[X], x: X): TestCluster[X] = new TestCluster(h.xs - x)

    override def posteriorPredictive(h: TestCluster[X], x: X): Double = 0

  }


}
