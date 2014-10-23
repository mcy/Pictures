package com.xorinc.pictures

import java.awt.image.BufferedImage
import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import javax.imageio.ImageIO

import com.google.gson.{GsonBuilder, JsonParser}
import org.apache.commons.lang3.StringEscapeUtils

import scala.collection.JavaConverters._
import scala.util.Random
import scala.util.control.NonFatal

object WikipediaEndpoint {

  private val articleCache = collection.mutable.Map.empty[String, Seq[String]]

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
  case class SomePictureData (
    name: String,
    img: BufferedImage,
    links: Seq[String],
    imgName: String,
    article: Seq[String]
  ) extends PictureData {
    val wordFreq = WordUtils.wordsByFreq(article.mkString(" "))
  }
  object NoPictureData extends PictureData
  object TitlePage extends SomePictureData (
    "Ascii Wikipedia Browser",
    ImageIO.read(this.getClass.getResourceAsStream("/titlecard.png")),
    Nil,
    "",
    Nil
  )

  private val randomCache = new collection.mutable.Stack[SomePictureData] {
    override def push(data: SomePictureData) : this.type       = synchronized(super.push(data))
    override def pop()                       : SomePictureData = synchronized(super.pop())
    override def size                        : Int             = synchronized(super.size)
  }
  private val randomTask = new Thread {
    override def run(): Unit = {
      while(true){
        if(randomCache.size < 5)
          try {
            randomCache.push(generateRandomPageData0())
          } catch {
            case NonFatal(e) => e.printStackTrace()
          }
        else {
          resetStatus()
          Thread.sleep(1000)
        }
      }
    }
  }
  randomTask.start()



  private def extract(query: String) = {
    setStatus(query)
    val url = new URL(query)
    val conn = url.openConnection()
    val reader = new BufferedReader(new InputStreamReader(conn.getInputStream))
    val data = Iterator.continually(reader.readLine()).takeWhile(_ ne null).mkString("\n")
    reader.close()
    data
  }

  def randomPage() = {
    val query = entryPoint + "?format=json&action=query&list=random&rnlimit=10&rnnamespace=0"
    val json = jparser.parse(extract(query)).getAsJsonObject
    val page =
      json.getAsJsonObject("query").getAsJsonArray("random").get(0).getAsJsonObject
    StringEscapeUtils.unescapeJson(page.getAsJsonPrimitive("title").getAsString)
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
    val array = json.getAsJsonArray.get(1).getAsJsonArray
    if(array.size() == 0)
      ""
    else
      array.get(0).getAsString
  }

  def resolveRedirects(title: String) = {
    val query = entryPoint +
      s"?action=query&format=json&titles=${title.replace(" ", "_")}&redirects"
    val json = jparser.parse(extract(query))
    json.getAsJsonObject
      .getAsJsonObject("query").getAsJsonObject("pages")
      .entrySet().asScala.head.getValue.getAsJsonObject
      .get("title").getAsString
  }

  val overuseCutoff = 40
  private def imageIsOverused(title: String): Boolean = {
    val query = entryPoint +
      s"?format=json&action=query&list=imageusage&iutitle=${title.replace(" ", "_")}&iulimit=$overuseCutoff&iunamespace=0"
    val json = jparser.parse(extract(query))
    val count = json.getAsJsonObject.getAsJsonObject("query").getAsJsonArray("imageusage").size
    count >= overuseCutoff
  }

  private def getPictureData0(_title: String, errorCallback: () => PictureData): PictureData = {
    val title = resolveRedirects(_title)
    val query = entryPoint +
      s"?format=json&action=query&titles=${title.replace(" ", "_")}&prop=images|links&plnamespace=0&pllimit=0"
    val _json = jparser.parse(extract(query))
    if(!_json.isJsonObject)
      return errorCallback()
    val json = _json.getAsJsonObject
    val page = json.getAsJsonObject("query").getAsJsonObject("pages").entrySet().asScala.head.getValue.getAsJsonObject
    val name = page.getAsJsonPrimitive("title").getAsString
    if(page.get("images") eq null){
      errorCallback()
    }
    else {
      val images = page.getAsJsonArray("images").iterator().asScala
        .filter(j => (j ne null) && j.getAsJsonObject.has("title"))
        .map(_.getAsJsonObject.getAsJsonPrimitive("title").getAsString)
        .filter(s => (s ne null) && !imageBlacklist(s) && imageExtensionWhitelist(s.substring(s.lastIndexOf(".")).toLowerCase))
        .filterNot(imageIsOverused)
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
        SomePictureData(name, bImage, links.toSeq, imageLoc, getArticleText(title.replace(" ", "_")))
      }
    }
  }

  val paragraph = """<p>(.+)</p>""".r
  //val citation = """<a href="#cite_note-.+"><span>\[</span>.+<span>\]</span></a>""".r
  val tag = """\<.*?\>""".r
  def getArticleText(title: String): Seq[String] = {
    articleCache.getOrElse(title, {
    val query = entryPoint +
      s"?action=parse&format=json&page=$title&prop=text&redirect"
    val json = jparser.parse(extract(query))
    val raw = json.getAsJsonObject.getAsJsonObject("parse").getAsJsonObject("text").get("*").getAsString
    val text = paragraph.findAllMatchIn(raw).map(_.group(1))//.toSeq
    val clean = text.map(s => tag.replaceAllIn(s, "").replaceAll("\\s+", " ")).filter(_.nonEmpty)
    val res = clean.map(StringEscapeUtils.unescapeHtml4).toSeq
    articleCache += (title -> res)
    res
    })
  }

  private val sectionBlacklist = Set("see also", "references", "sources", "further reading", "external links")
  def getSections(title: String, count: Int = 3) = {
    val query = entryPoint + s"?action=parse&format=json&page=$title&prop=sections"
    val json = jparser.parse(extract(query))
    val sections =
      json.getAsJsonObject.getAsJsonObject("parse").getAsJsonArray("sections")
      .iterator().asScala.toSeq.map(_.getAsJsonObject).filterNot(o => sectionBlacklist(o.get("line").getAsString.toLowerCase))
    rand.shuffle(sections).take(count).map(_.get("index").getAsInt)
  }

  def getPictureData(title: String): PictureData = {
    getPictureData0(searchForArticle(title.replace(' ', '_')), () => NoPictureData)
  }

  private object FetchLock

  def generateRandomPageData(): SomePictureData =
    if(randomCache.size == 0)
      FetchLock synchronized {
        randomCache.pop()
      }
    else randomCache.pop()

  // alas, no tailrec because reasons. let the stack overflow begin

  private def generateRandomPageData0(): SomePictureData = {
    FetchLock synchronized {
      val title = randomPage()
      getPictureData0(title, () => {
        Thread.sleep(3000)
        generateRandomPageData0()
      }).asInstanceOf[SomePictureData]
    }
  }

  val wikiURLPrefix = "http://en.wikipedia.org/wiki/"
  def articleURL(title: String) = new URL(wikiURLPrefix + title.replace(' ', '_'))

  def resetStatus() = Main.gui.resetStatus()
  def setStatus(message: String) = Main.gui.setStatus(message)
}
