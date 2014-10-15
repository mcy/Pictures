package com.xorinc.pictures

import java.io.{InputStreamReader, BufferedReader}

object WordUtils {

  private val commonWords = {
    val in = new BufferedReader(new InputStreamReader(this.getClass.getResourceAsStream("/common_words.txt")))
    val set = Iterator.continually({
      in.readLine()
    }).takeWhile(_ ne null).toSet
    in.close()
    set + ""
  }

  def wordsByFreq(text: String) = {
    val map = weightMap(text)
    map.toSeq.sortBy(_._2).reverse
  }

  def weightMap(text: String): collection.mutable.Map[String, Int]= {
    import collection.mutable
    if(text.isEmpty) return mutable.Map.empty[String, Int]
    val words = text.split("[\\s-—]+").map{ s =>
      s.toLowerCase.replace("'s", "").replaceAll("""[^\p{IsAlphabetic}]""", "")
    }.filterNot(commonWords(_)).filter(_.nonEmpty).sorted
    //println(words.mkString("\n"))
    val counts = mutable.Map.empty[String, Int]
    var last = words.head
    var count = 0
    for(w <- words){
      if(w != last){
        counts(w) = count
        last = w
        count = 0
      }
      count += 1
    }
    counts
  }
}
