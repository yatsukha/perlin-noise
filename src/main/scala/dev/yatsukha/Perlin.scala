package dev.yatsukha

package object noise {
  import conversions._
  import util._

  // x, y and _1, _2 accessors are used to convey semantic meaning
  // ie it does not make sense for x bounds to be addressed with x and y
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

}