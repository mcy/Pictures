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

      private var pageIndex = 0

      def updateArrows() = {
        prev.setEnabled(currentIndex > 0)
        next.setEnabled(currentIndex < history.length - 1)
        prevSection.setEnabled(pageIndex > 0)
        nextSection.setEnabled(pageIndex < now.img.length - 1)
        linkButton.setEnabled(now.links.size > 0)
      }

      def updateSectionArrows() = {
      }

      def back() = {
        currentIndex -= 1
        pageIndex = 0
        refresh()
      }
      def forward() = {
        currentIndex += 1
        pageIndex = 0
        refresh()
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
          makeNewImage(now, pageIndex)
        updateArrows()
      }

      def now = history(currentIndex)

      def nextPage() = {
        pageIndex += 1
        refresh()
      }

      def prevPage() = {
        pageIndex -= 1
        refresh()
      }
    }


    val properties = new {

      var inverted: Boolean = false
      var fontSize: Int = 5
      var overlay: Boolean = false
    }

    import JFrame._
    import GuiUtils._

    setTitle("Wikipicture")
    //setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
    setDefaultCloseOperation(EXIT_ON_CLOSE)

    val mainPanel = new JPanel
    mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS))
    this.add(mainPanel)

    var preferedSize = (100, 450)



    val console = new JTextArea(preferedSize._1, preferedSize._2)
    console.setFont(new Font("Menlo", Font.PLAIN, 5))
    console.setEditable(false)

    var consoleDim: Dimension = console.getSize()

    def updateSize(): Unit = {
      preferedSize = ((consoleDim.getHeight / fontHeight).toInt, (consoleDim.getWidth / fontWidth).toInt)
      console.setRows(preferedSize._1)
      console.setColumns(preferedSize._2)
      //println(preferedSize)
      console.setSize(consoleDim)
    }

    def updateFont(): Unit = {
      console.setFont(new Font("Menlo", Font.PLAIN, properties.fontSize))
      updateFontMetrics()
      updateSize()
    }

    var metrics: FontMetrics = _
    var fontHeight, fontWidth: Int = _
    def updateFontMetrics(): Unit = {
      metrics = console.getFontMetrics(console.getFont)
      fontWidth = metrics.charWidth(' ')
      fontHeight = Math.ceil(Ascii.aspectRatio * fontWidth).toInt
      fontWidth = (fontWidth * 1.05).toInt
      fontHeight = Math.ceil(fontHeight * 1.4).toInt
      //println((fontWidth, fontHeight))
    }; updateFontMetrics()

    mainPanel.add (console)
    console.setBackground(Color.BLACK)
    console.setForeground(Color.WHITE)

    def updateColors(): Unit = {
      if(properties.inverted){
        console.setForeground(Color.BLACK)
        console.setBackground(Color.WHITE)
      } else {
        console.setForeground(Color.WHITE)
        console.setBackground(Color.BLACK)
      }
    }

    private val status = new JLabel(" ")
    status.setFont(new Font("Menlo", Font.PLAIN, 6))

    def resetStatus() = setStatus(" ")
    def setStatus(message: String) =
      status synchronized {
        status.setText(message)
      }

    mainPanel.add (
      new JPanel {
        this.setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS))
        this.add(Box.createHorizontalGlue())
        this.add(status)
        this.add(Box.createHorizontalGlue())
      }
    )

    private val controls = new JPanel

    mainPanel.add(controls)
    
    val newButton = new JButton("Random Page")
    newButton act {
      makeNewImage(WikipediaEndpoint.generateRandomPageData(), 0)
    }

    controls.add(newButton)

    controls.add (
      panel (BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)) { p =>

        val articleField = new JTextField(35)
        articleField.setFont(new Font("Menlo", Font.PLAIN, 12))

        def query() = {
          WikipediaEndpoint.getPictureData(articleField.getText) match {
            case x: SomePictureData => makeNewImage(x, 0)
            case NoPictureData => articleField.setText("No picture! =(")
          }
        }

        articleField typed { c =>
          if(c == '\n')
            query()
        }
        p.add(articleField)

        val articleButton = new JButton("Go")
        articleButton act {
          query()
        }
        p.add(articleButton)
      }
    )

    val prev = new JButton("\u25C0")
    val next = new JButton("\u25B6")

    controls.add (
      panel (BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)) { p =>
        p add new JLabel("Article")
        prev.setEnabled(false)
        prev act {
          History.back()
        }
        p.add(prev)

        next.setEnabled(false)
        next act {
          History.forward()
        }
        p.add(next)
      }
    )

    val prevSection = new JButton("\u25C0")
    val nextSection = new JButton("\u25B6")

    controls.add (
      panel (BorderFactory.createEtchedBorder(EtchedBorder.LOWERED)) { p =>
        p add new JLabel("Page")
        prevSection.setEnabled(false)
        prevSection act {
          History.prevPage()
        }
        p.add(prevSection)

        nextSection.setEnabled(false)
        nextSection act {
          History.nextPage()
        }
        p.add(nextSection)
      }
    )

    val optionButton = new JButton("Options")

    optionButton act {
      new JFrame {
        val _p = new JPanel
        _p.setLayout(new BoxLayout(_p, BoxLayout.Y_AXIS))
        this.add(_p.asInstanceOf[Container])
        def add(pan: JPanel): Unit = {
          val _pan = new JPanel
          _pan.setLayout(new BoxLayout(_pan, BoxLayout.X_AXIS))
          _pan.add(pan)
          _pan.add(Box.createHorizontalGlue())
          _p.add(_pan)
        }
        def _add(pan: JPanel): Unit = {
          val _pan = new JPanel
          _pan.setLayout(new BoxLayout(_pan, BoxLayout.X_AXIS))
          _pan.add(pan)
          _pan.add(Box.createHorizontalGlue())
          _p.add(_pan)
        }

        val over = new JCheckBox
        val inv = new JCheckBox
        val sizes = new JComboBox[java.lang.Integer]((5 to 12).map(Integer.valueOf).toArray)

        over.setSelected(properties.overlay)
        inv.setSelected(properties.inverted)
        sizes.setSelectedIndex(properties.fontSize - 5)

        val apply = new JButton("Apply")
        val cancel = new JButton("Cancel")

        apply act {

          properties.overlay = over.isSelected
          properties.inverted = inv.isSelected
          properties.fontSize = sizes.getSelectedIndex + 5

          updateColors()
          updateFont()
          History.refresh()

          //this.dispose()
        }

        cancel act {
          this.dispose()
        }

        add(makePanel(over, new JLabel("Article Overlay")))
        add(makePanel(inv, new JLabel("Invert Console Colors")))
        add(makePanel(sizes, new JLabel("Font Size")))
       _add(makePanel(apply, cancel))


        pack()
        setResizable(false)
        setLocationRelativeTo(null)
        setVisible(true)
      }
    }

    controls.add(optionButton)

    val linkButton = new JButton("Links")

    linkButton act {
      new JFrame {
        val pan = new JPanel
        pan.setLayout(new BoxLayout(pan, BoxLayout.Y_AXIS))
        this.add(pan)
        pan.add(Box.createVerticalStrut(10))
        val pan2 = new JPanel
        pan2.setLayout(new BoxLayout(pan2, BoxLayout.X_AXIS))
        pan2.add(Box.createHorizontalStrut(10))

        val selector = new JList[String](History.now.links.toArray.padTo(10, ""))
        selector.setLayoutOrientation(JList.VERTICAL)
        selector.setVisibleRowCount(10)
        selector.setPrototypeCellValue("-" * 50)
        selector.setForeground(Color.BLUE)
        selector onSelect { e =>
          val selected = selector.getModel.getElementAt(e.getFirstIndex)
          if(selected.nonEmpty) {
            this.dispose()
            WikipediaEndpoint.getPictureData(selected) match {
              case x: SomePictureData => History(x); History.refresh()
              case _ => throw new Exception(s"Bad link '$selected'")
            }
          }
        }

        val scroll = new JScrollPane(selector)

        pan2.add(scroll)
        pan2.add(Box.createHorizontalStrut(10))
        pan.add(pan2)
        pan.add(Box.createVerticalStrut(10))

        pack()
        setTitle("Article Links")
        setLocationRelativeTo(null)
        setResizable(false)
        setVisible(true)
      }
    }

    controls.add(linkButton)

    val openButton = desktop.map(_ => new JButton("Open in Browser"))

    openButton foreach { openButton =>
      openButton act {
        val title =
          if(History.now == TitlePage)
            "Main Page"
          else
            History.now.name
        openWebpage(articleURL(title))
      }
      controls.add(openButton)
    }

    //this.setSize(screenWidth, screenHeight - 24)
    //setResizable(false)
    pack()

    val controlPanelHeight = (this.getSize().getHeight - console.getSize().getHeight).toInt
    println(controlPanelHeight)

    this.setMinimumSize(this.getSize)
    setLocationRelativeTo(null)
    //setVisible(true)

    this.addComponentListener(
      onResize { e =>
        console.setSize(new Dimension(console.getWidth, this.getHeight - controlPanelHeight))
        consoleDim = console.getSize()
        updateSize()
        History.refresh()
      }
    )

    History(TitlePage)

    def consoleSize = (console.getColumns, console.getRows)

    def makeNewImage(data: SomePictureData, index: Int): Unit = {
      updateSize()
      val img = Ascii.scaleToFit(data.img(index), consoleSize)
      val picture = Ascii.toAscii(img)
      setTitle(data.name)
      History(data)
      val text = Ascii.center(
        if(properties.overlay)
          picture
        else
          Ascii.insertArticle(
            Ascii.sprinkleKeywords(
              picture,
              data.wordFreq.take(10).map(_._1)
            ),
            data.articleSlice(index)
          ),
        consoleSize
      )
      //println(data.article)
      History.updateSectionArrows()
      console.setText(text)
    }

    def init() = {
      History.refresh()
      this.setVisible(true)
    }
  }

  def main(args: Array[String]): Unit = {
    gui.init()
  }
}