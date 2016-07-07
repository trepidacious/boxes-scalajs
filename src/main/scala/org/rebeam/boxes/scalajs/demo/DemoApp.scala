package org.rebeam.boxes.scalajs.demo

import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExport

import scala.scalajs.js.JSApp

import scalatags.JsDom.all._
import org.rebeam.boxes.scalajs._

import monocle._
import monocle.macros.GenLens

case class Street(name: String, number: Int)
case class Address(street: Street)
case class Company(address: Address)
case class Employee(name: String, company: Company)

object Street {
  val _name: Lens[Street, String]  = GenLens[Street](_.name)
  val _number: Lens[Street, Int]  = GenLens[Street](_.number)
}

object Address {
  val _street: Lens[Address, Street]  = GenLens[Address](_.street)
}

object Company {
  val _address: Lens[Company, Address]  = GenLens[Company](_.address)
}

object Employee {
  val _name: Lens[Employee, String]  = GenLens[Employee](_.name)
  val _company: Lens[Employee, Company]  = GenLens[Employee](_.company)
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
    
    val e = Employee("bob", Company(Address(Street("bobstreet", 42))))
    
    val streetName = (Employee._company ^|-> Company._address ^|-> Address._street ^|-> Street._name).get(e)
    
    appendPar(document.body, streetName)
    
    appendPar(document.body, "Hello World")

    document.body.appendChild(view(e).render)
    
    val lv = listView[String]
    
    document.body.appendChild(div(lv.render(List("a", "b", "c"))).render)
  }
}
