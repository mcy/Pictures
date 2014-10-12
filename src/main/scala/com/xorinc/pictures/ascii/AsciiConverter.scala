package com.xorinc.pictures.ascii

import java.awt.image.BufferedImage
import java.io.InputStreamReader

import com.google.gson.JsonParser
import com.xorinc.pictures.Main

import scala.collection.JavaConverters._
import scala.util.Random

object AsciiConverter {

  private val rand = new Random
  private val (colors, aspectRatio) = {
    val jparser = new JsonParser
    val stream = this.getClass.getResourceAsStream("/palette.json")
    val json = jparser.parse(new InputStreamReader(stream))
    stream.close()
    val asp = json.getAsJsonObject.get("aspect_ratio").getAsDouble
    val arr = json.getAsJsonObject.getAsJsonObject("charset").entrySet().asScala.toArray.map{e =>
      (e.getValue.getAsDouble, e.getKey()(0))
    }
    val max = arr.maxBy(_._1)._1
    (arr.map(t => (t._1 / max, t._2)), asp)
  }

  def center(s: String, canvas: (Int, Int)) = {
    val lines = s.split("\n")
    val sidePadding = "~" * ((canvas._1 - lines(0).length)/2)
    val topPadding = "~" * ((canvas._2 - lines.length)/2)
    topPadding + lines.map(sidePadding + _ + sidePadding).mkString("\n") + topPadding
  }

  def scaleToFit(img: BufferedImage, canvas: (Int, Int)) = {

    val scale = Math.min(canvas._1.toDouble / img.getWidth, canvas._2.toDouble * aspectRatio / img.getHeight)
    resizeImage(img, (scale * aspectRatio, scale))

  }

  def resizeImage(img: BufferedImage, scale: (Double, Double)) = {
    val itype = if(img.getType == 0) BufferedImage.TYPE_INT_ARGB else img.getType
    val resized = new BufferedImage((img.getWidth * scale._1 + 0.5).toInt, (img.getHeight * scale._2 + 0.5).toInt, itype)
    val gfx = resized.createGraphics()
    gfx.drawImage(img, 0, 0, resized.getWidth, resized.getHeight, null)
    gfx.dispose()
    resized
  }

  def insertWords(image: String, words: Seq[String]) = {
    val replacements = rand.shuffle(words.distinct).take(100)
    replacements.fold(image){ (i, r) =>
      val reps = rand.shuffle(s"[A-Z]{${r.length}}".r.findAllMatchIn(i).toSeq)
      if(reps.isEmpty){
        i
      }
      else {
        val rep = reps.head
        val buffer = new StringBuilder(i)
        buffer.replace(rep.start, rep.end, r)
        buffer.toString()
      }

    }
  }

  def toAscii(image: BufferedImage): String = {
    val invert = Main.gui.invert.isSelected
    val chars =
      for(y <- Iterator.tabulate((image.getHeight/aspectRatio).toInt)(_ * aspectRatio))
        yield for(x <- 0 until image.getWidth)
          yield {
            val hsl = toHSL({
              val nextInt = (y + 0.5).toInt
              val upper = toTuple(image.getRGB(x, nextInt))
              val lower = toTuple(image.getRGB(x, nextInt + 1))
              val upperLength = nextInt - y
              val lowerLength = y + aspectRatio - nextInt
              (
                (upper._1 * upperLength + lower._1 * lowerLength)./(2).toInt,
                (upper._2 * upperLength + lower._2 * lowerLength)./(2).toInt,
                (upper._3 * upperLength + lower._3 * lowerLength)./(2).toInt
                )
            })._3
            if(invert)
              getChar(1 - hsl)
            else
              getChar(hsl)
          }
    chars.map(_.mkString("")).mkString("\n")
  }

  def getChar(color: Int): Char = getChar(color / 255.0)
  def getChar(color: Double) = {
    def next(d: Double, l: Array[(Double, Char)]): Char = {
      val mid = (l.length - 1)/2
      if(l(mid)._1 == d) l(mid)._2
      else if(l.length == 2){
        val (d1, c1) = l(0)
        val (d2, c2) = l(1)
        if(d - d1 < d2 - d)
          c1
        else c2
      } else if (l(mid)._1 > d)
        next(d, l.takeRight(l.length/2))
      else
        next(d, l.take(l.length/2))
    }
    next(color, colors)
  }

  def toTuple(color: Int) = (color >> 16 & 0xff, color >> 8 & 0xff, color & 0xff)
  def toHSL(color: Int): (Double, Double, Double) = {
    toHSL(toTuple(color))
  }
  def toHSL(color: (Int, Int, Int)): (Double, Double, Double) = {
    val r = color._1/255.0
    val g = color._2/255.0
    val b = color._3/255.0

    val max = Math.max(Math.max(r, g), b)
    val min = Math.min(Math.min(r, g), b)

    var h, s, l = (max + min) / 2

    if(max == min){
      h = 0
      s = 0
    } else {
      val d = max - min
      s = if (l > 0.5) d / (2 - max - min) else d / (max + min)
      h =
        if(max == r)
         (g - b) / d + (if(g < b) 6 else 0)
        else if(max == g)
         (b - r) / d + 2
        else if(max == b)
         (r - g) / d + 4
        else -1
    }
    (h, s, l)
  }
}
