package org.rebeam.boxes.scalajs.demo

import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExport

import scala.scalajs.js.JSApp

import scalatags.JsDom.all._
import org.rebeam.boxes.scalajs._

object DemoApp extends JSApp {

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    targetNode.appendChild(p(text).render)
  }

  @JSExport
  def addClickedMessage(): Unit = {
    appendPar(document.body, "You clicked the button!")
  }

  def main(): Unit = {
    import View._
    
    appendPar(document.body, "Hello World")
    
    val lv = listView[String]
    
    document.body.appendChild(div(lv.render(List("a", "b", "c"))).render)
  }
}
