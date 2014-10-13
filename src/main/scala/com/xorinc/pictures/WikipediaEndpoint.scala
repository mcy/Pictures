package com.xorinc.pictures

import java.awt.image.BufferedImage
import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import javax.imageio.ImageIO

import com.google.gson.{GsonBuilder, JsonParser}
import org.apache.commons.lang.StringEscapeUtils

import scala.collection.JavaConverters._
import scala.util.Random

object WikipediaEndpoint {

  val entryPoint = "http://en.wikipedia.org/w/api.php"
  val jparser = new JsonParser
  val gson = new GsonBuilder().setPrettyPrinting().create()
  val rand = new Random

  private val imageBlacklist = {
    val stream = this.getClass.getResourceAsStream("/imageBlacklist.json")
    val json = jparser.parse(new InputStreamReader(stream))
    json.getAsJsonArray.asScala.map(_.getAsJsonPrimitive.getAsString).toSet
  }
  private val imageExtensionWhitelist = Set(".png", ".jpg", ".jpeg")

  sealed trait PictureData
  case class SomePictureData(name: String, img: BufferedImage, links: Seq[String], imgName: String) extends PictureData
  object NoPictureData extends PictureData

  private def extract(query: String) = {
    val url = new URL(query)
    val conn = url.openConnection()
    val reader = new BufferedReader(new InputStreamReader(conn.getInputStream))
    val data = Iterator.continually(reader.readLine()).takeWhile(_ ne null).mkString("\n")
    reader.close()
    data
  }

  def randomPage() = {
    val query = entryPoint + "?format=json&action=query&list=random&rnlimit=1&rnnamespace=0"
    val json = jparser.parse(extract(query)).getAsJsonObject
    val page =
      json.getAsJsonObject("query").getAsJsonArray("random").get(0).getAsJsonObject
    StringEscapeUtils.unescapeJavaScript(page.getAsJsonPrimitive("title").getAsString)
  }

  def getImageUrl(url: String) = {
    val query =
      entryPoint + s"?format=json&action=query&titles=${url.replace(" ", "_")}&prop=imageinfo&iiprop=url"
    val data = extract(query)
    val page =
      jparser.parse(data).getAsJsonObject.getAsJsonObject("query")
        .getAsJsonObject("pages").entrySet().asScala.head.getValue.getAsJsonObject
    page.getAsJsonArray("imageinfo").get(0).getAsJsonObject.getAsJsonPrimitive("url").getAsString
  }

  def searchForArticle(search: String) = {
    val query = entryPoint + s"?format=json&action=opensearch&search=$search&namespace=0&limit=1&format=json"
    val data = extract(query)
    val json = jparser.parse(data)
    //println(gson.toJson(json))
    val array = json.getAsJsonArray.get(1).getAsJsonArray
    if(array.size() == 0)
      ""
    else
      array.get(0).getAsString
  }


  private def getPictureData0(title: String, errorCallback: () => PictureData): PictureData = {
    val query = entryPoint +
      s"?format=json&action=query&titles=${title.replace(" ", "_")}&redirects&prop=images|links&plnamespace=0&pllimit=450"
    val _json = jparser.parse(extract(query))
    if(!_json.isJsonObject)
      return errorCallback()
    val json = _json.getAsJsonObject
    val page = json.getAsJsonObject("query").getAsJsonObject("pages").entrySet().asScala.head.getValue.getAsJsonObject
    //println(gson.toJson(json))
    val name = page.getAsJsonPrimitive("title").getAsString
    if(page.get("images") eq null){
      errorCallback()
    }
    else {
      val images = page.getAsJsonArray("images").iterator().asScala
        .filter(j => (j ne null) && j.getAsJsonObject.has("title"))
        .map(_.getAsJsonObject.getAsJsonPrimitive("title").getAsString)
        .filter(s => (s ne null) && !imageBlacklist(s) && imageExtensionWhitelist(s.substring(s.lastIndexOf(".")).toLowerCase))
        .toSeq
      if (images.isEmpty) {
        errorCallback()
      }
      else {
        val imageLoc = images(if(Main.gui.useRandom.isSelected) rand.nextInt(images.size) else 0)
        val bImage = ImageIO.read(new URL(getImageUrl(imageLoc)))
        val links = page.getAsJsonArray("links").asScala
          .map(_.getAsJsonObject.getAsJsonPrimitive("title").getAsString
          .replaceAll("[^A-Za-z ]|\\(.+\\)", "").trim.replace(" ", "*"))
          .filter(_ != "")
        SomePictureData(name, bImage, links.toSeq, imageLoc)
      }
    }
  }

  def getPictureData(title: String): PictureData = {
    getPictureData0(searchForArticle(title.replace(" ", "_")), () => NoPictureData)
  }

  // alas, no tailrec because reasons. let the stack overflow begin
  def generateRandomPageData(): SomePictureData = {
    val title = randomPage()
    getPictureData0(title, () => {
      Thread.sleep(3000)
      generateRandomPageData()
    }).asInstanceOf[SomePictureData]
  }
}
