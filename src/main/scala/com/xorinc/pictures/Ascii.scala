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

  private val chars = colors.zipWithIndex.map(t => (t._1._2, t._2)).toMap
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

  def processArticle(raw: Seq[String]): Seq[String] = {
    import collection.mutable
    //println(raw.mkString("\n"))
    val res = mutable.ArrayBuffer.empty[String]
    val buffer = new StringBuilder()
    for(s <- raw){
      buffer.append(" " + s)
      if(rand.nextBoolean()){
        res += buffer.toString().trim
        buffer.clear()
      }
    }
    //println(res.mkString("\n"))
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
    //println(" " + (width, height))
    val shuffled = rand.shuffle(text).iterator
    var countdown = 7 //total regions!
    def setChar(c: Char, x: Int, y: Int) = buf.setCharAt((x - 1) + y * (width+1), c)
    while(countdown > 0 && shuffled.hasNext) { breakable {
      val locW = rand.nextInt(width)
      val locH = rand.nextInt(height)
      //println((locW, locH))
      val c = lines(locH)(locW)
      val allowedColors = {
        val slice = colors.filter(t => (t._1 - char2val(c)).abs < 0.05D)
        slice.map(_._2).toSet
      }
      //println(allowedColors)
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
        val min = sorted.head
        val max = sorted.last
        val zipped = sorted.zip(sorted.tail)
        val seq =
          for(((x1, y), (x2, _)) <- zipped)
            yield
              if(Math.abs(x1 - x2) < 3){
                for(x <- x1 until x2)
                  yield (x, y)
              }
              else
                (x1, y) :: Nil
        seq.flatten

      }).filter(_.size > 7)

      val region = regionUnflattened.flatten.sortBy(t => t._1 + t._2 * width)

      //val start = rand.nextInt(region.maxBy(_._2)._2 - length)
      //val modRegion = region.filter(_._2 > start)
      if (minRegionSize > region.size || region.size > maxRegionSize) break()
      var offset = 0
      if (!shuffled.hasNext) break()
      var s = shuffled.next()
      for (index <- 0 until region.size) {
        //println((s, s.length))
        if (index - offset > s.length - 1) {
          if (!shuffled.hasNext)
            break()
          offset += s.length
          s = " " + shuffled.next()
        }
        val char = s(index - offset)
        //print((index-offset, char))
        val loc = region(index) //modRegion(index)
        setChar(char, loc._1, loc._2)
      }

      for(line <- regionUnflattened){
        for(segment <- {
          def newSeq = mutable.ArrayBuffer.empty[(Int, Int)]
          val segments = mutable.ArrayBuffer.empty[Seq[(Int, Int)]]
          var seg: mutable.ArrayBuffer[(Int, Int)] = null
          var last: (Int, Int) = null
          for(loc <- line){
            if(seg eq null){
              seg = newSeq
            } else if(loc._1 - last._1 > 1){
              segments += seg
              seg = newSeq
            }
            seg += loc
            last = loc
          }
          segments
        }) {
          val min = segment.head
          val max = segment.last
          if (min._1 > 0)
            setChar(outerLeftChar, min._1 - 1, min._2)
          if (max._1 < width - 1)
            setChar(outerRightChar, max._1 + 1, max._2)
        }
      }
      //println
      usedPoints ++ region
      countdown -= 1
      /*println({
        val set = region.toSet
        Array.tabulate(height, width)((y, x) => if(set((y, x))) "*" else " ").map(_.mkString).mkString("\n")
      })*/
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
    val chars =
      for(y <- Iterator.tabulate((image.getHeight/aspectRatio).toInt)(_ * aspectRatio))
        yield for(x <- 0 until image.getWidth)
          yield {
            val hsl = lightness({
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
            })
            if(invert)
              getChar(hsl)
            else
              getChar(1-hsl)
          }
    chars.map(_.mkString("")).mkString("\n")
  }

  private def pixel(image: BufferedImage, x: Int, y: Double) = {
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
  }

  def getChar(color: Int): Char = getChar(color / 255.0)
  def getChar(color: Double) = {
    def next(d: Double, l: Array[(Double, Char)]): Char = {
      //println(l.mkString("[", ",", "]"))
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

  private def toTuple(color: Int) = (color >> 16 & 0xff, color >> 8 & 0xff, color & 0xff)
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

  def lightness(color: Int): Double = lightness(toTuple(color))
  def lightness(color: (Int, Int, Int)): Double = {
    val r = color._1/255.0
    val g = color._2/255.0
    val b = color._3/255.0

    val max = Math.max(Math.max(r, g), b)
    val min = Math.min(Math.min(r, g), b)

    (max + min)/2
  }
}
