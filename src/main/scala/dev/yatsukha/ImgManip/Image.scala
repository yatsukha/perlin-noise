package dev.yatsukha

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

sealed class Image(size: (Int, Int)) {
  def width  = size._1
  def height = size._2

  private val img = 
    new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  def get(xy: (Int, Int)): Int =
    img.getRGB(xy._1, xy._2)

  def set(xy: (Int, Int))(rgb: (Int, Int, Int)): Unit = 
    img.setRGB(xy._1, xy._2, new Color(rgb._1, rgb._2, rgb._3).getRGB)

  def write(file: String): Unit =
    ImageIO.write(img, file.split('.')(1), new File(file))
    
}