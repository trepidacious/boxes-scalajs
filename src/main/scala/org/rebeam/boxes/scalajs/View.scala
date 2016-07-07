package org.rebeam.boxes.scalajs
import scalatags.JsDom.all._

/**
  * A view of a given model type (or subclass) as a given tag type
  */
trait View[-M] {
  def render(m: M): Tag
}

object View {
  implicit val stringView = new View[String] {
    def render(m: String) = p(m)
  }
  implicit val intView = new View[Int] {
    def render(m: Int) = p(m.toString)
  }
  implicit def listView[A: View] = new View[List[A]] {
    def render(m: List[A]) = ul(m.map(a => li(implicitly[View[A]].render(a))))
  }
  def view[M: View](m: M) = implicitly[View[M]].render(m)
}
