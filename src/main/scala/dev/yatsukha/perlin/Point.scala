package dev.yatsukha.perlin

sealed case class Point[A](p: (A, A))(implicit ev: Numeric[A]) {
  import ev._

  def x = p._1
  def y = p._2

  def scale(weight: A): A =
    (ev.fromInt(1) - weight) * x + weight * y

  def *(other: Point[A]): A =
    x * other.x + this.y * other.y
}

package object conversions {
  implicit def tupleToPoint[A: Numeric](p: (A, A)): Point[A] = Point(p)
}
