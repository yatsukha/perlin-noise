package dev.yatsukha

package object noise {
  import conversions._
  import util._

  def noise(g: IndexedSeq[IndexedSeq[(Double, Double)]], p: (Double, Double)): Double = {
    // floors and ceils of the point, aka bounds
    val bx  = (p.x.toInt, (p.x + 1).toInt)
    val by  = (p.y.toInt, (p.y + 1).toInt)

    // distances to the bounds
    val dx  = (p.x - bx._1, p.x - bx._2)
    val dy  = (p.y - by._1, p.y - by._2)

    // smooth step of the upperleft bound is used as weight
    val w   = (smoothStep(dx._1), smoothStep(dy._1))

    // dot product for upper bounds
    var vx0 = g(bx._1)(by._1) * (dx._1, dy._1)
    var vx1 = g(bx._2)(by._1) * (dx._2, dy._1)

    val vy0 = (vx0, vx1).scale(w.x)

    // dot product for lower bounds
        vx0 = g(bx._1)(by._2) * (dx._1, dy._2)
        vx1 = g(bx._2)(by._2) * (dx._2, dy._2)

    val vy1 = (vx0, vx1).scale(w.x)

    (vy0, vy1).scale(w.y)
  }

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

    math.min(math.max(value / s.toDouble, -1.0), 1.0)
  }

}