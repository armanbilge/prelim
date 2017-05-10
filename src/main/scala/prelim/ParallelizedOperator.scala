package prelim

import mcmc.Operator

import scala.collection.GenSeq

class ParallelizedOperator[T](op: Operator[T, Double]) extends Operator[GenSeq[T], Double] {

  override def apply(t: GenSeq[T]): GenSeq[T] = t.par.map(op)

  override def hastingsRatio(x: GenSeq[T], y: GenSeq[T]): Double = (x, y).zipped.map(op.hastingsRatio).sum

}
