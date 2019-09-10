package dev.yatsukha

package object util {

  def smoothStep(x: Double): Double =
    x * x * (3 - 2 * x)

}