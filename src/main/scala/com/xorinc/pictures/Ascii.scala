package com.xorinc.pictures

import java.awt.image.BufferedImage
import java.io.InputStreamReader

import com.google.gson.JsonParser

import scala.collection.JavaConverters._
import scala.util.Random

object Ascii {

  private val rand = new Random
  private val (colors, aspectRatio) = {
    val jparser = new JsonParser
    val stream = this.getClass.getResourceAsStream("/palette.json")
    val json = jparser.parse(new InputStreamReader(stream))
    stream.close()
    val asp = json.getAsJsonObject.get("aspect_ratio").getAsDouble
    val arr = json.getAsJsonObject.getAsJsonObject("charset").entrySet().asScala.toArray.map{e =>
      (e.getValue.getAsDouble, e.getKey()(0))
    }.sortBy(_._1)
    val max = arr.maxBy(_._1)._1
    (arr.map(t => (t._1 / max, t._2)), asp)
  }

  //private val chars = colors.zipWithIndex.map(t => (t._1._2, t._2)).toMap
  private val char2val = colors.map(_.swap).toMap

  def center(s: String, canvas: (Int, Int)): String = {
    val lines = s.split("\n")
    val sidePadding = " " * ((canvas._1 - lines(0).length)/2)
    val topPadding = (" " * Main.gui.consoleSize._1 + "\n") * ((canvas._2 - lines.length)/2)
    topPadding + lines.map(sidePadding + _ + sidePadding).mkString("\n") + topPadding
  }

  def scaleToFit(img: BufferedImage, canvas: (Int, Int)) = {

    val baseScale = (canvas._1.toDouble / img.getWidth).min(aspectRatio * canvas._2.toDouble / img.getHeight)
    val scale =
      if(img.getHeight.toDouble / img.getWidth < aspectRatio * canvas._2.toDouble / canvas._1)
        (baseScale, baseScale / aspectRatio)
      else
        (baseScale * aspectRatio, baseScale)
    rescaleImage(img, scale)
  }

  def resizeImage(img: BufferedImage, size: (Int, Int)) = {
    val itype = if(img.getType == 0) BufferedImage.TYPE_INT_ARGB else img.getType
    val resized = new BufferedImage(size._1, size._2, itype)
    val gfx = resized.createGraphics()
    gfx.drawImage(img, 0, 0, resized.getWidth, resized.getHeight, null)
    gfx.dispose()
    resized
  }

  def rescaleImage(img: BufferedImage, scale: (Double, Double)) = {
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
      val reps = rand.shuffle(s"[A-Za-z$$]{${r.length}}".r.findAllMatchIn(i).toSeq)
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

  def distance(a: String, b: String): Double = {
    val zip = a.zip(b.toCharArray).map{ t =>
      if(!char2val.contains(t._1) || !char2val.contains(t._1))
        Double.MaxValue
      else
        (char2val(t._1) - char2val(t._2)).abs
    }//.sum /// a.length
    zip.sum / a.length
  }

  val distanceCutoff = 0.25D //magic number!
  def sprinkleKeywords(image: String, words: Seq[String]): String = {
    import collection.mutable
    if(words.isEmpty) return image
    val buf = new StringBuilder(image)
    val usedPoints = mutable.Set.empty[(Int, Int)]
    val lines = image.split("\n")
    val width = lines(0).length
    val height = lines.length
    var hardCountdown = 50

    def setString(s: String, x: Int, y: Int) = {
      val loc = (x - 1) + y * (width + 1)
      if(0 <= loc && loc < buf.length)
        buf.replace(loc, loc + s.length, s)
    }

    for((w, i) <- words.zipWithIndex){
        for(_ <- 1 to (words.size - i) * 8) {
          var best = (Double.MaxValue, (-1, -1))
          while (hardCountdown > 0){

            val locW = rand.nextInt(width - w.length - 2) + 1
            val locH = rand.nextInt(height)

            val region = lines(locH).substring(locW, locW + w.length)


            if(!(locW to locW + w.length).map((_, locH)).exists(usedPoints(_))) {
              val dist = distance(w, region)
              if (dist < best._1) {
                best = (dist, (locW, locH))
              }
            }
            hardCountdown -= 1
          }
          hardCountdown = 50
          if(best._1 <= distanceCutoff) {
            setString(w, best._2._1, best._2._2)
            usedPoints ++= (best._2._1 - 1 to best._2._1 + w.length + 1) map ((_, best._2._2))
          }
        }
    }
    buf.toString()
  }

  def processArticle(raw: Seq[String]): Seq[String] = {
    import collection.mutable
    val res = mutable.ArrayBuffer.empty[String]
    val buffer = new StringBuilder()
    for(s <- raw){
      buffer.append(" " + s)
      if(rand.nextBoolean()){
        res += buffer.toString().trim
        buffer.clear()
      }
    }
    rand.shuffle(res).take(5)
  }

  val maxRegionSize = 1500
  val minRegionSize = 150
  val outerLeftChar  = '∎'
  val outerRightChar = '∎'

  def insertArticle(image: String, text: Seq[String]): String = {
    import collection.mutable
    import util.control.Breaks._
    if(text.isEmpty) return image
    val usedPoints = mutable.Set.empty[(Int, Int)]
    val buf = new StringBuilder(image)
    val lines = image.split("\n")
    val width = lines(0).length
    val height = lines.length
    val shuffled = rand.shuffle(text).iterator
    var hardCountdown = 50
    var countdown = 7 //total regions!

    def setChar(c: Char, x: Int, y: Int) = {
      val loc = (x - 1) + y * (width+1)
      if(0 <= loc && loc < buf.length)
        buf.setCharAt(loc, c)
    }

    while(hardCountdown > 0 && countdown > 0 && shuffled.hasNext) { breakable {
      hardCountdown -= 1
      val locW = rand.nextInt(width)
      val locH = rand.nextInt(height)
      val c = lines(locH)(locW)
      if(!char2val.contains(c) || char2val(c) < 0.2)
        break()
      val allowedColors = {
        val slice = colors.filter(t => (t._1 - char2val(c)).abs < 0.05D)
        slice.map(_._2).toSet
      }
      val rawregion = {
        val set = mutable.Set.empty[(Int, Int)]
        val pixels = mutable.Stack[(Int, Int)]()

        def add(x: Int, y: Int) = set += ((x, y))
        def old(x: Int, y: Int) =
          allowedColors(lines(y)(x)) && !set((x, y)) && !usedPoints((x, y))
        def push(x: Int, y: Int) = pixels.push((x, y))
        def next = pixels.pop()

        push(locW, locH)

        while (pixels.nonEmpty) {
          val (x, y) = next
          var y1 = y
          while (y1 >= 0 && old(x, y1)) y1 -= 1
          y1 += 1
          var spanLeft, spanRight = false
          while (y1 < height && old(x, y1)) {
            add(x, y1)
            if (x > 0 && spanLeft != old(x - 1, y1)) {
              if (old(x - 1, y1)) push(x - 1, y1)
              spanLeft = !spanLeft
            }
            if (x < width - 1 && spanRight != old(x + 1, y1)) {
              if (old(x + 1, y1)) push(x + 1, y1)
              spanRight = !spanRight
            }
            y1 += 1
          }
        }
        set//.toSeq
      }
      val splitByY = rawregion.groupBy(_._2).values.toSeq
      val regionUnflattened = splitByY.map({ s =>
        val sorted = s.toSeq.sortBy(_._1)
        //val min = sorted.head
        //val max = sorted.last
        val zipped = sorted.zip(sorted.tail).toIndexedSeq
        val seq =
          for(((x1, y), (x2, _)) <- zipped)
            yield
              if(Math.abs(x1 - x2) < 6){
                for(x <- x1 until x2)
                  yield (x, y)
              }
              else
                (x1, y) :: Nil
        seq.flatten//.toSeq
      }).filter(_.size > 7)
      val region = regionUnflattened.flatten.sortBy(t => t._1 + t._2 * width)

      //val start = rand.nextInt(region.maxBy(_._2)._2 - length)
      //val modRegion = region.filter(_._2 > start)
      if (minRegionSize > region.size || region.size > maxRegionSize) break()
      var offset = 0
      if (!shuffled.hasNext) break()
      var s = shuffled.next()
      for (index <- 0 until region.size) {
        if (index - offset > s.length - 1) {
          if (!shuffled.hasNext)
            break()
          offset += s.length
          s = " " + shuffled.next()
        }
        val char = s(index - offset)
        val loc = region(index)
        setChar(char, loc._1, loc._2)
      }

      usedPoints ++ region
      countdown -= 1
    }}

    for(i <- 1 until height){
      buf.setCharAt(i * (width+1) - 1, '\n')
    }
    buf.toString()
  }

  def toLightnessMap(image: BufferedImage): Array[Array[Double]] = {
    val result = Array.fill(image.getWidth)(Array.fill(image.getHeight)(0.0))
    for(x <- 0 until image.getWidth; y <- 0 until image.getHeight){
      result(x)(y) = lightness(image.getRGB(x, y))
    }
    result
  }

  def toAscii(image: BufferedImage): String = {
    val invert = Main.gui.invert.isSelected
    val background = toTuple(if(invert) 0xffffffff else 0xff000000)
    val chars =
      for(y <- Iterator.tabulate((image.getHeight/aspectRatio).toInt)(_ * aspectRatio))
        yield {
          val nextInt = (y + aspectRatio).toInt
          val upperLength = (y - nextInt).abs
          val lowerLength = y + aspectRatio - nextInt
          for(x <- 0 until image.getWidth)
          yield {
            val hsl = lightness({
              val upper = toTuple(image.getRGB(x, nextInt))
              val lower = toTuple(image.getRGB(x, Math.min(nextInt + 1, image.getHeight - 1)))
              val alpha = (upper._4 * upperLength + lower._4 * lowerLength) / 2 / 255.0

              (
                ((upper._1 * upperLength + lower._1 * lowerLength) / 2 * alpha + background._1 * (1 - alpha)).toInt,
                ((upper._2 * upperLength + lower._2 * lowerLength) / 2 * alpha + background._2 * (1 - alpha)).toInt,
                ((upper._3 * upperLength + lower._3 * lowerLength) / 2 * alpha + background._3 * (1 - alpha)).toInt,
                1
              )
            })
            if(invert)
              getChar(1-hsl)
            else
              getChar(hsl)
          }
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
      } else if (l(mid)._1 < d)
        next(d, l.takeRight(l.length/2))
      else
        next(d, l.take(l.length/2))
    }
    next(color, colors)
  }


  private def toTuple(color: Int) = (color >> 16 & 0xff, color >> 8 & 0xff, color & 0xff, color >> 24 & 0xff)
  def toHSL(color: Int): (Double, Double, Double, Double) = {
    toHSL(toTuple(color))
  }
  def toHSL(color: (Int, Int, Int, Int)): (Double, Double, Double, Double) = {
    val r = color._1/255.0
    val g = color._2/255.0
    val b = color._3/255.0
    val alpha = color._4/255.0

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
    (h, s, l, alpha)
  }

  def lightness(color: Int): Double = {
    lightness(toTuple(color))
  }
  def lightness(color: (Int, Int, Int, Int)): Double = {
    val r = color._1/255.0
    val g = color._2/255.0
    val b = color._3/255.0
    val alpha = color._4/255.0

    val max = Math.max(Math.max(r, g), b)
    val min = Math.min(Math.min(r, g), b)

    (max + min) / 2
  }
}
