package dev.yatsukha

object Main extends App {
  if (args.length != 3) {
    println("args: rows columns ticks")
    System.exit(-1)
  }

  val r = args(0).toInt
  val c = args(1).toInt
  val t = args(2).toInt

  println(s"image resolution: ${c * t}x${r * t}")

  val gf = generators.gradientField(
    new scala.util.Random(System.currentTimeMillis), 
    (r + 1, c + 1)
  )

  val perlin = 
    (0 until (r * t))
      .map(_.toDouble / t.toDouble)
      .map(
        x =>
          (0 until (c * t))
            .map(_.toDouble / t.toDouble)
            .map(y => noise.noise(gf, (x, y)))
            .map(util.exaggerate(_))
      )

  val image = new Image(c * t, r * t)

  for (x <- perlin.indices)
    for (y <- perlin(x).indices) {
      val eq = x.toDouble * 5.0 / perlin.length.toDouble +
               y.toDouble * 2.0 / perlin(x).length.toDouble +
               0.25 * util.turbulence(perlin, 32)(x, y)

      val color = math.abs(math.sin(eq * Math.PI)).toFloat
      image.set(x, y)((212.0 * color).toInt, (125.0 * color).toInt, (65.0 * color).toInt)
    }

  image.write("test.jpg")
}