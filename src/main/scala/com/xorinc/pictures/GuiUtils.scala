package com.xorinc.pictures

import java.awt.event._
import java.awt.{Component, Desktop, GridBagConstraints, GridBagLayout}
import java.net.{URL, URI}
import javax.swing.event.{ListSelectionListener, ListSelectionEvent}
import javax.swing.{JList, JLabel, JCheckBox, JPanel}
import javax.swing.border.Border

object GuiUtils {

  import java.awt.GridBagConstraints._
  val North = NORTH
  val South = SOUTH
  val West = WEST
  val East = EAST
  val Center = CENTER
  val Northwest = NORTHWEST
  val Northeast = NORTHEAST
  val Southwest = SOUTHWEST
  val Southeast = SOUTHEAST

  def Layout(
    x: Int,
    y: Int,
    width: Int = 1,
    height: Int = 1,
    anchor: Int = GridBagConstraints.CENTER
  ) = {
    val layout = new GridBagConstraints()
    layout.gridx = x
    layout.gridy = y
    layout.gridwidth = width
    layout.gridheight = height
    layout.anchor = anchor
    layout
  }

  implicit class lambdaActionListener[A <: {
    def addActionListener(a: ActionListener): Unit
  }](val self: A) extends AnyVal {
    def actOn[?](f: ActionEvent => ?) = {
      self.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = f(e)
      })
      self
    }
    def act[?](f: => ?) = {
      self.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = f
      })
      self
    }
  }

  implicit class lambdaMouseListener[A <: {
    def addMouseListener(a: MouseListener): Unit
  }](val self: A) extends AnyVal {
    def clickOn[?](f: MouseEvent => ?) = {
      self.addMouseListener(new MouseAdapter {
        override def mouseClicked(e: MouseEvent): Unit = f(e)
      })
      self
    }
    def click[?](f: => ?) = {
      self.addMouseListener(new MouseAdapter {
        override def mouseClicked(e: MouseEvent): Unit = f
      })
      self
    }
  }

  implicit class lambdaKeyListener[A <: {
    def addKeyListener(a: KeyListener): Unit
  }](val self: A) extends AnyVal {
    def typed[?](f: Char => ?) = {self.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = f(e.getKeyChar)

      override def keyPressed(e: KeyEvent): Unit = ()

      override def keyReleased(e: KeyEvent): Unit = ()
    }); self}

    def pressed[?](f: Char => ?) = {self.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = f(e.getKeyChar)

      override def keyReleased(e: KeyEvent): Unit = ()
    }); self}

    def released[?](f: Char => ?) = {self.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = ()

      override def keyReleased(e: KeyEvent): Unit = f(e.getKeyChar)
    }); self}

    def keys[_1, _2, _3](typed: Char => _1, pressed: Char => _2, released: Char => _2) =
    {self.addKeyListener(new KeyListener {

      override def keyTyped(e: KeyEvent): Unit = typed(e.getKeyChar)

      override def keyPressed(e: KeyEvent): Unit = pressed(e.getKeyChar)

      override def keyReleased(e: KeyEvent): Unit = pressed(e.getKeyChar)
    }); self}

    def typeEvent[?](f: KeyEvent => ?) = {self.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = f(e)

      override def keyPressed(e: KeyEvent): Unit = ()

      override def keyReleased(e: KeyEvent): Unit = ()
    }); self}

    def pressEvent[?](f: KeyEvent => ?) = {self.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = f(e)

      override def keyReleased(e: KeyEvent): Unit = ()
    }); self}

    def releaseEvent[?](f: KeyEvent => ?) = {self.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = ()

      override def keyReleased(e: KeyEvent): Unit = f(e)
    }); self}

    def keyEvent[_1, _2, _3](typed: KeyEvent => _1, pressed: KeyEvent => _2, released: KeyEvent => _2) =
    {self.addKeyListener(new KeyListener {

      override def keyTyped(e: KeyEvent): Unit = typed(e)

      override def keyPressed(e: KeyEvent): Unit = pressed(e)

      override def keyReleased(e: KeyEvent): Unit = pressed(e)
    }); self}
  }

  def panel[?](border: Border)(f: JPanel => ?) = {
    val panel = new JPanel
    panel.setLayout(new GridBagLayout)
    f(panel)
    panel.setBorder(border)
    panel
  }

  def checkboxAct[?](name: String)(f: JCheckBox => ?) = {
    val box = new JCheckBox
    box act {
      f(box)
    }
    val pan = panel(null) { p =>
      val label = new JLabel(name)
      label click {
        box.setSelected(!box.isSelected)
        f(box)
      }
      p.add(label)
      p.add(box)
    }
    (box, pan)
  }
  def checkbox[?](name: String) = {
    val box = new JCheckBox
    val pan = panel(null) { p =>
      val label = new JLabel(name)
      label click {
        box.setSelected(!box.isSelected)
      }
      p.add(label)
      p.add(box)
    }
    (box, pan)
  }

  lazy val desktop = {
    if(Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)){
      Some(Desktop.getDesktop)
    } else None
  }

  def openWebpage(uri: URI) = desktop.foreach(_.browse(uri))

  def openWebpage(url: URL): Unit = openWebpage(url.toURI)

  def onResize(f: ComponentEvent => Unit) = new ComponentAdapter {
    override def componentResized(e: ComponentEvent): Unit = f(e)
  }

  def makePanel(comps: Component*): JPanel = {
    val p = new JPanel
    for(comp <- comps)
      p.add(comp)
    p
  }

  def onClose(f: WindowEvent => Unit) = new WindowAdapter {
    override def windowClosed(e: WindowEvent): Unit = f(e)
  }

  implicit class OnSelect(val l: JList[_]) extends AnyVal {

    def onSelect(f: ListSelectionEvent => Unit): Unit = l.addListSelectionListener(new ListSelectionListener {
      override def valueChanged(e: ListSelectionEvent): Unit = f(e)
    })
  }

}
