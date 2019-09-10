package dev.yatsukha

package object util {

  def smoothStep(x: Double): Double =
    x * x * (3 - 2 * x)

  def exaggerate(x: Double, a: Double = 2.0, limits: (Double, Double) = (-1.0, 1.0))
    : Double = math.max(math.min(x * a, limits._2), limits._1)

  def smooth(n: IndexedSeq[IndexedSeq[Double]])(xy: (Double, Double)): Double = {
    val indices  = (xy._1.toInt, xy._2.toInt)
    val distance = (xy._1 - indices._1.toDouble, xy._2 - indices._2.toDouble)

    val dxdy = Vector(
      (0, 0, 1 - distance._1, 1 - distance._2),
      (0, 1, 1 - distance._1, distance._2),
      (1, 0, distance._1, 1 - distance._2),
      (1, 1, distance._1, distance._2)
    )

    var value = 0.0

    for (idx <- dxdy) {
      val pos = (indices._1 + idx._1, indices._2 + idx._2)
      value = value + n(pos._1 % n.length)(pos._2 % n.head.length) * idx._3 * idx._4
    }

    value
  }

  def turbulence(n: IndexedSeq[IndexedSeq[Double]], s: Int)(xy: (Int, Int)): Double = {
    var counter = s
    var value = 0.0

    while (counter >= 1) {
      value = value + smooth(n)(
          xy._1.toDouble / counter.toDouble, 
          xy._2.toDouble / counter.toDouble
        ) * counter

      counter = counter / 2
    }

    // TODO: diagnose this function
    math.min(math.max(value / s.toDouble, -1.0), 1.0)
  }

}