package dev.yatsukha

object Main extends App {
  import scala.util.Random
  import generators._

  println(gradientField(new Random(-1), (5, 5)))
}