package org.rebeam.boxes.scalajs.demo

import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExport

import scala.scalajs.js.JSApp

import scalatags.JsDom.all._
import org.rebeam.boxes.scalajs._

import monocle._
import monocle.macros.{GenLens, Lenses, PLenses}

import upickle.Js
import upickle.Invalid
import upickle.default._

@Lenses case class Street(name: String, number: Int)
@Lenses case class Address(street: Street)
@Lenses case class Company(address: Address)
@Lenses case class Employee(name: String, company: Company)

object DeltaReaders {
  implicit def deltaReaderFromReader[M: Reader] = new DeltaReader[M] {
    def read(v: Js.Value) = SetDelta(implicitly[Reader[M]].read(v))
  }
}

object Street {
  import DeltaReaders._
  
  implicit val streetDeltaReader: DeltaReader[Street] = new DeltaReader[Street] {
    def read(v: Js.Value): Delta[Street] = v match {
      case Js.Obj(field, _ @ _*) => field match {
        case ("lens", v) => v match {
          case Js.Obj(field, _ @ _*) => field match {
            case ("name", v) => LensDelta(Street.name, implicitly[DeltaReader[String]].read(v))
            case ("number", v) => LensDelta(Street.number, implicitly[DeltaReader[Int]].read(v))
            case _ => throw new Invalid.Data(v, "Invalid delta, expected object with name or number field")
          }
        }
        case ("set", v) => SetDelta(implicitly[Reader[Street]].read(v))
        
        case _ => throw new Invalid.Data(v, "Invalid delta, expected object with name or number field")
      }
      case _ => throw new Invalid.Data(v, "Invalid delta, expected object")
    }
  }
}

object Address {
  import DeltaReaders._
  import Street._
  
  implicit val addressDeltaReader: DeltaReader[Address] = new DeltaReader[Address] {
    def read(v: Js.Value): Delta[Address] = v match {
      case Js.Obj(field, _ @ _*) => field match {
        case ("lens", v) => v match {
          case Js.Obj(field, _ @ _*) => field match {
            case ("street", v) => LensDelta(Address.street, streetDeltaReader.read(v))
            case _ => throw new Invalid.Data(v, "Invalid delta, expected object with street field")
          }
        }
        case ("set", v) => SetDelta(implicitly[Reader[Address]].read(v))
        
        case _ => throw new Invalid.Data(v, "Invalid delta, expected object with name or number field")
      }

      case _ => throw new Invalid.Data(v, "Invalid delta, expected object")
    }
  }

}

object Company {
}

object Employee {
}

trait Delta[M] {
  def apply(m: M): M
}

case class LensDelta[A, B](lens: Lens[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): A = lens.modify(delta.apply)(a)
}

case class OptionalDelta[A, B](optional: Optional[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): A = optional.modify(delta.apply)(a)
}

case class SetDelta[M](v: M) extends Delta[M] {
  def apply(m: M): M = v
}

trait DeltaReader[M] {
  def read(v: Js.Value): Delta[M]
}

object DemoApp extends JSApp {

  import View._

  implicit val streetView = new View[Street] {
    def render(m: Street) = div(p("Street:"), view(m.name), view(m.number))
  }

  implicit val addressView = new View[Address] {
    def render(m: Address) = div(p("Address:"), view(m.street))
  }

  implicit val companyView = new View[Company] {
    def render(m: Company) = div(p("Company:"), view(m.address))
  }

  implicit val employeeView = new View[Employee] {
    def render(m: Employee) = div(p("Employee:"), view(m.name), view(m.company))
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    targetNode.appendChild(p(text).render)
  }

  @JSExport
  def addClickedMessage(): Unit = {
    appendPar(document.body, "You clicked the button!")
  }

  def main(): Unit = {
    
    val a = Address(Street("OLD STREET", 1))
    
    val a2 = Address.addressDeltaReader.read(
      Js.Obj("lens" -> 
        Js.Obj("street" ->
          Js.Obj("lens" -> 
            Js.Obj("name" -> Js.Str("Specified new street name!"))
          )
        )
      )
    ).apply(a)

    val a3 = Address.addressDeltaReader.read(
      Js.Obj("lens" -> 
        Js.Obj("street" ->
          Js.Obj("lens" -> 
            Js.Obj("number" -> Js.Num(42))
          )
        )
      )
    ).apply(a2)

    val setDeltaJSON =       
      Js.Obj("lens" -> 
        Js.Obj("street" ->
          Js.Obj("set" -> implicitly[Writer[Street]].write(Street("Completely new set street", 9001))
          )
        )
      )


    val a4 = Address.addressDeltaReader.read(setDeltaJSON).apply(a3)
    

    appendPar(document.body, "Before delta: " + a)
    appendPar(document.body, "After name delta: " + a2)
    appendPar(document.body, "After number delta: " + a3)
    appendPar(document.body, "After set delta: " + a4)
    appendPar(document.body, "Example JSON delta: " + setDeltaJSON)
    
    val e = Employee("bob", Company(Address(Street("bobstreet", 42))))
    
    val streetName = (Employee.company ^|-> Company.address ^|-> Address.street ^|-> Street.name).get(e)
    
    appendPar(document.body, streetName)
    
    appendPar(document.body, "Hello World")

    document.body.appendChild(view(e).render)
    
    val lv = listView[String]
    
    document.body.appendChild(div(lv.render(List("a", "b", "c"))).render)
  }
}
