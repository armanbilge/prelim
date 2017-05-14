package prelim

import com.sun.jna.Native

object gsl {

  Native.register("gsl")

  def Wm1(x: Double): Double = gsl_sf_lambert_Wm1(x)

  @native def gsl_sf_lambert_Wm1(x: Double): Double

}
