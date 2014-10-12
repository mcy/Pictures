package com.xorinc.pictures.gui

import java.awt.{GridBagLayout, GridBagConstraints}
import java.awt.event.{ActionListener, ActionEvent}
import javax.swing.JPanel
import javax.swing.border.Border

object GuiUtils {

  import GridBagConstraints._
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

  def panel[?](border: Border)(f: JPanel => ?) = {
    val panel = new JPanel
    panel.setLayout(new GridBagLayout)
    f(panel)
    panel.setBorder(border)
    panel
  }
}
