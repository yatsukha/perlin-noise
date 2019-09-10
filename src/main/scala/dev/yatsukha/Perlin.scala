package dev.yatsukha

package object noise {
  import conversions._
  import util._

  def noise(g: IndexedSeq[IndexedSeq[(Double, Double)]], p: (Double, Double)): Double = {
    val bx  = (p.x.toInt, (p.x + 1).toInt)
    val by  = (p.y.toInt, (p.y + 1).toInt)

    val dx  = (p.x - bx._1, p.x - bx._2)
    val dy  = (p.y - by._1, p.y - by._2)

    val w   = (smoothStep(dx._1), smoothStep(dx._2))

    var vx0 = g(bx._1)(by._1) * (dx._1, dy._1)
    var vx1 = g(bx._2)(by._1) * (dx._2, dy._1)

    val vy0 = (vx0, vx1).scale(w.x)

        vx0 = g(bx._1)(by._2) * (dx._1, dy._2)
        vx1 = g(bx._2)(by._2) * (dx._2, dy._2)

    val vy1 = (vx0, vx1).scale(w.x)

    (vy0, vy1).scale(w.y)
  }

}