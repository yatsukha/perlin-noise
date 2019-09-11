package dev.yatsukha

package object util {

  def smoothStep(x: Double): Double =
    x * x * (3 - 2 * x)

  def exaggerate(x: Double, a: Double = 1.85, limits: (Double, Double) = (-1.0, 1.0))
    : Double = math.max(math.min(x * a, limits._2), limits._1)

  def marble(noise: Double)
            (xy: (Double, Double))
            (noiseStrength: Double = 0.25, period: (Double, Double) = (5.0, 10.0)): Double =
    (1 + math.sin((noise * noiseStrength + xy._1 * period._1 + xy._2 * period._2) * math.Pi)) / 2

}