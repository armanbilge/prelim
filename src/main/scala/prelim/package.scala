package object prelim {

  def solveForAlpha(K: Int, n: Int): Double =
    - K * n / (K + n * gsl.Wm1(- K * math.exp(- K.toDouble / n) / n))

}
