package dev.yatsukha

sealed case class Point[A: Fractional](p: (A, A)) {
  import Fractional.Implicits._

  def x = p._1
  def y = p._2

  def scale(weight: A)(implicit ev: Numeric[A]): A = {
    (ev.fromInt(1) - weight) * x + weight * y
  }

  def *(other: Point[A]): A =
    x * other.x + y * other.y
}

package object conversions {
  implicit def tupleToPoint[A: Fractional](p: (A, A)): Point[A] = 
    Point(p)
}
