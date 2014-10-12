package com.xorinc.pictures

import java.awt.event.ActionEvent
import java.awt.{GridBagConstraints, Font, GridBagLayout}
import java.io.{FileReader, BufferedReader, File}
import javax.imageio.ImageIO
import javax.swing._
import javax.swing.border.EtchedBorder

import com.xorinc.pictures.ascii.{AsciiConverter => Ascii}
import com.xorinc.pictures.wiki.WikipediaEndpoint
import com.xorinc.pictures.wiki.WikipediaEndpoint.{NoPictureData, SomePictureData}

import scala.collection.mutable

object Main {



  lazy val gui = new JFrame() {

    object History {

      import collection.mutable.ArraySeq

      private val history = mutable.ArrayBuffer[SomePictureData]()

      private var currentIndex = -1

      def back() = {
        currentIndex -= 1
        makeNewImage(history(currentIndex))
        if(currentIndex <= 0)
          prev.setEnabled(false)
        if(currentIndex < history.length - 1)
          next.setEnabled(true)
      }
      def forward() = {
        currentIndex += 1
        makeNewImage(history(currentIndex))
        if(currentIndex > 0)
          prev.setEnabled(true)
        if(currentIndex >= history.length - 1)
          next.setEnabled(false)
      }

      def apply(s: SomePictureData) = {
        if(history.find(p => p.name == s.name && p.img == s.img).isEmpty) {
          history += s
          currentIndex = history.size - 1
          prev.setEnabled(history.length > 1)
          next.setEnabled(false)
        }
      }
    }

    import JFrame._
    import com.xorinc.pictures.gui.GuiUtils._

    setTitle("Wikipicture")
    setLayout(new GridBagLayout)
    setDefaultCloseOperation(EXIT_ON_CLOSE)

    val console = new JTextArea(100, 450)
    console.setFont(new Font("Menlo", Font.PLAIN, 5))
    console.setEditable(false)

    val metrics = console.getFontMetrics(console.getFont)

    add (
      new JScrollPane (
        console,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED
      ),
      Layout (
        x = 0,
        y = 0,
        width = 6,
        anchor = North
      )
    )

    val newButton = new JButton("New Image")
    newButton act {
      makeNewImage(WikipediaEndpoint.generateRandomPageData())
    }

    add (
      newButton,
      Layout(
        x = 0,
        y = 1,
        anchor = East
      )
    )

    add (
      panel (BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)) { p =>

        val articleField = new JTextField(35)
        articleField.setFont(new Font("Menlo", Font.PLAIN, 12))
        p.add (
          articleField,
          Layout(
            x = 0,
            y = 0
          )
        )

        val articleButton = new JButton("Fetch Page")
        articleButton act {
          WikipediaEndpoint.getPictureData(articleField.getText) match {
            case x: SomePictureData => makeNewImage(x)
            case NoPictureData => articleField.setText("No picture! =(")
          }
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
        y = 1,
        anchor = West
      )
    )

    val prev = new JButton("<-")
    val next = new JButton("->")

    add (
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
        y = 1,
        anchor = West
      )
    )

    val useRandom = new JCheckBox()

    add (
      panel (null) { p =>
        p.add(
          new JLabel("  Use random article image:"),
          Layout(
            x = 0,
            y = 0
          )
        )
        p.add(
          useRandom,
          Layout(
            x = 1,
            y = 0
          )
        )
      },
      Layout(
        x = 3,
        y = 1,
        anchor = West
      )
    )

    val invert = new JCheckBox()

    add (
      panel (null) { p =>
        p.add(
          new JLabel(" Invert colors:"),
          Layout(
            x = 0,
            y = 0
          )
        )
        p.add(
          invert,
          Layout(
            x = 1,
            y = 0
          )
        )
      },
      Layout(
        x = 4,
        y = 1,
        anchor = West
      )
    )

    setResizable(false)
    pack()
    setLocationRelativeTo(null)
    setVisible(true)

    def consoleSize = (console.getColumns, console.getRows)

    def makeNewImage(data: SomePictureData) = {
      val img = Ascii.scaleToFit(data.img, consoleSize)
      val picture = Ascii.toAscii(img)
      setTitle(data.name)
      History(data)
      console.setText(Ascii.center(Ascii.insertWords(picture, data.links), consoleSize))
    }
  }

  def main(args: Array[String]): Unit = {
    gui
  }
}