package prelim

import mcmc.Operator

class ParallelizedOperator[T](op: Operator[T, Double]) extends Operator[IndexedSeq[T], Double] {

  override def apply(t: IndexedSeq[T]): IndexedSeq[T] = t.par.map(op).to

  override def hastingsRatio(x: IndexedSeq[T], y: IndexedSeq[T]): Double = (x, y).zipped.map(op.hastingsRatio).sum

}
