package com.xorinc.pictures

import java.awt._
import javax.imageio.ImageIO
import javax.swing._
import javax.swing.border.EtchedBorder
import WikipediaEndpoint._

import scala.collection.mutable

object Main {

  lazy val gui = new JFrame() {

    lazy val (screenWidth, screenHeight) = {
      val toolkit = Toolkit.getDefaultToolkit
      val size = toolkit.getScreenSize
      (size.getWidth.toInt, size.getHeight.toInt)
    }

    object History {

      private val history = mutable.ArrayBuffer.empty[SomePictureData]

      private var currentIndex = 0


      def updateArrows() = {
        prev.setEnabled(currentIndex > 0)
        next.setEnabled(currentIndex < history.length - 1)
      }

      def back() = {
        currentIndex -= 1
        makeNewImage(history(currentIndex))
        updateArrows()
      }
      def forward() = {
        currentIndex += 1
        makeNewImage(history(currentIndex))
        updateArrows()
      }

      def apply(s: SomePictureData) = {
        history.zipWithIndex.find(p => p._1.name == s.name && p._1.imgName == s.imgName) match {
          case Some((_, index)) => currentIndex = index
          case None =>
            history += s
            currentIndex = history.size - 1
        }
        updateArrows()
      }

      def refresh() = {
        if(currentIndex >= 0)
          makeNewImage(history(currentIndex))
      }

      def now = history(currentIndex)
    }

    import JFrame._
    import GuiUtils._

    setTitle("Wikipicture")
    setLayout(new BorderLayout)
    setDefaultCloseOperation(EXIT_ON_CLOSE)

    val console = new JTextArea(100, 450)
    console.setFont(new Font("Menlo", Font.PLAIN, 5))
    console.setEditable(false)

    val metrics = console.getFontMetrics(console.getFont)

    add (
      console/*new JScrollPane (
        console,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED
      )*/,
      BorderLayout.CENTER
    )

    val controls = new JPanel(new GridBagLayout)

    add (
      controls,
      BorderLayout.SOUTH
    )
    
    val newButton = new JButton("Random Page")
    newButton act {
      makeNewImage(WikipediaEndpoint.generateRandomPageData())
    }

    controls.add (
      newButton,
      Layout(
        x = 0,
        y = 0,
        anchor = East
      )
    )

    controls.add (
      panel (BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)) { p =>

        val articleField = new JTextField(35)
        articleField.setFont(new Font("Menlo", Font.PLAIN, 12))

        def query() = {
          WikipediaEndpoint.getPictureData(articleField.getText) match {
            case x: SomePictureData => makeNewImage(x)
            case NoPictureData => articleField.setText("No picture! =(")
          }
        }

        articleField typed { c =>
          if(c == '\n')
            query()
        }

        p.add (
          articleField,
          Layout(
            x = 0,
            y = 0
          )
        )

        val articleButton = new JButton("Fetch Page")
        articleButton act {
          query()
        }
        p.add (
          articleButton,
          Layout(
            x = 1,
            y = 0
          )
        )


      },
      Layout(
        x = 1,
        y = 0,
        anchor = West
      )
    )

    val prev = new JButton("<-")
    val next = new JButton("->")

    controls.add (
      panel (BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)) { p =>
        prev.setEnabled(false)
        prev act {
          History.back()
        }
        p.add(
          prev,
          Layout(
            x = 0,
            y = 0
          )
        )

        next.setEnabled(false)
        next act {
          History.forward()
        }
        p.add(
          next,
          Layout(
            x = 1,
            y = 0
          )
        )
      },
      Layout(
        x = 2,
        y = 0,
        anchor = West
      )
    )

    val (useRandom, randomPan) = checkbox("  Random image:")

    //disabled because the images are not sent in order anyways.
    useRandom.setSelected(true)
    /*controls.add (
      randomPan,
      Layout(
        x = 3,
        y = 0,
        anchor = West
      )
    )*/

    val (invert, invertPan) = checkboxAct("  Invert colors:") { box =>
      if(box.isSelected){
        console.setBackground(Color.BLACK)
        console.setForeground(Color.WHITE)
      } else {
        console.setBackground(Color.WHITE)
        console.setForeground(Color.BLACK)
      }
      History.refresh()
    }

    controls.add (
      invertPan,
      Layout(
        x = 4,
        y = 0,
        anchor = West
      )
    )

    val (overlay, overlayPan) = checkboxAct(" Disable article overlay:") { box =>
      History.refresh()
    }

    controls.add (
      overlayPan,
      Layout(
        x = 5,
        y = 0,
        anchor = West
      )
    )

    val reloadButton = new JButton("Redraw")
    reloadButton act {
      History.refresh()
    }

    controls.add (
      reloadButton,
      Layout(
        x = 6,
        y = 0,
        anchor = East
      )
    )
    
    if(desktopSupported) {
      val openButton = new JButton("Open in Browser")
      openButton act {
        if(History.now != TitlePage)
        openWebpage(articleURL(History.now.name))
      }
      controls.add(
        openButton,
        Layout(
          x = 7,
          y = 0,
          anchor = East
        )
      )
    }
    //this.setSize(screenWidth, screenHeight - 24)
    setResizable(false)
    pack()
    setLocationRelativeTo(null)
    setVisible(true)

    History(TitlePage)

    def consoleSize = (console.getColumns, console.getRows)

    def makeNewImage(data: SomePictureData): Unit = {
      val img = Ascii.scaleToFit(data.img, consoleSize)
      val picture = Ascii.toAscii(img)
      setTitle(data.name)
      History(data)
      val text = Ascii.center(if(overlay.isSelected) picture else Ascii.insertArticle(picture, data.article), consoleSize)
      //println(data.article)
      console.setText(text)
    }

    def init() = History.refresh()
  }

  def main(args: Array[String]): Unit = {
    gui.init()
  }
}