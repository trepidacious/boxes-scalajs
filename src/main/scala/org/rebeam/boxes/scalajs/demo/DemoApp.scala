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

import scala.language.higherKinds

@Lenses case class Street(name: String, number: Int)
@Lenses case class Address(street: Street)
@Lenses case class Company(address: Address)
@Lenses case class Employee(name: String, company: Company)

sealed trait StreetAction extends Delta[Street]

case class StreetActionNumberMultiple(multiple: Int) extends StreetAction {
  def apply(s: Street): Street = Street(s.name, s.name.length * multiple)
}

object DeltaReaders {
  def deltaReaderFromReader[M: Reader] = new DeltaReader[M] {
    def readDelta(v: Js.Value) = ValueDelta(implicitly[Reader[M]].read(v))
  }
  def deltaReaderFromPF[M](error: String)(pf: PartialFunction[Js.Value, Delta[M]]) = new DeltaReader[M] {
    def readDelta(v: Js.Value): Delta[M] = pf.applyOrElse(v, (v: Js.Value) => throw new Invalid.Data(v, error))
  }
}

object Street {
  import DeltaReaders._
  
  implicit val streetDeltaReader: DeltaReader[Street] = new DeltaReader[Street] {
    def readDelta(v: Js.Value): Delta[Street] = v match {
      case Js.Obj(field, _ @ _*) => field match {
        case ("lens", v) => v match {
          case Js.Obj(field, _ @ _*) => field match {
            case ("name", v) => LensDelta(Street.name, deltaReaderFromReader[String].readDelta(v))
            case ("number", v) => LensDelta(Street.number, deltaReaderFromReader[Int].readDelta(v))
            case _ => throw new Invalid.Data(v, "Invalid delta, expected object with name or number field")
          }
        }
        
        case ("value", v) => ValueDelta(implicitly[Reader[Street]].read(v))
        
        case ("action", v) => implicitly[Reader[StreetAction]].read(v)
        
        case _ => throw new Invalid.Data(v, "Invalid delta, expected object with field lens, set or action")
      }
      case _ => throw new Invalid.Data(v, "Invalid delta, expected object")
    }
  }
}

/**
 * Interface provided by a parent component to a child component, 
 * allowing it to run a delta, which is also required encoded to JSON.
 * This is all the child component needs to know about a parent component.
 * The child's model itself (of type C) will be passed separately.
 */
trait Parent[C] {
  def runDelta(delta: Delta[C], deltaJs: Js.Value): Unit
}

/**
 * The parent at the root of a model of type R. This handles deltas by 
 * calling a callback. This can be used by a view of the root of a model
 * to produce Parent instances for its children using Parent implementations
 * that expect a Parent themselves.
 */
case class RootParent[R](callback: (Delta[R], Js.Value) => Unit) extends Parent[R] {
  def runDelta(delta: Delta[R], deltaJs: Js.Value): Unit = callback(delta, deltaJs)
}

/**
 * Produce a parent for a child component, using a lens for a named field to
 * reach that child's model.
 */ 
case class LensParent[P, C](parent: Parent[P], lens: Lens[P, C], fieldName: String) extends Parent[C] {
  def runDelta(delta: Delta[C], deltaJs: Js.Value): Unit = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = LensDelta(lens, delta)
    
    //Add this delta to the JSON
    val parentDeltaJs = Js.Obj("lens" -> Js.Obj(fieldName -> deltaJs))
    
    //Run using the parent's own parent
    parent.runDelta(parentDelta, parentDeltaJs)    
  }
}

/**
 * Cursor into a root data model, giving the model at the current location and
 * a parent to be used to run deltas to update the model, as well as means to
 * zoom into the model to produce cursors for child data
 */
case class Cursor[M](parent: Parent[M], model: M) extends Parent[M] {
  
  //Just pass through runDelta to parent for convenience
  def runDelta(delta: Delta[M], deltaJs: Js.Value): Unit = parent.runDelta(delta, deltaJs)
  
  def act[A <: Delta[M]](actionDelta: A)(implicit writer: Writer[A]): Unit = 
    runDelta(actionDelta, Js.Obj("action" -> writer.write(actionDelta)))

  def set(newModel: M)(implicit writer: Writer[M]): Unit = 
    runDelta(ValueDelta(newModel), Js.Obj("value" -> writer.write(newModel)))
  
  //TODO if we had a FieldLens being a Lens with an added fieldName: String we could use this instead, and
  //use a macro to provide these (and readDelta implementation)
  //TODO macros could generate a specific cursor type (e.g. AddressCursor) for each model type, having
  //methods to zoom to the cursor for each field, providing the appropriate child cursor
  //type for that child (e.g. StreetCursor), when that child is also using the same macro?
  //This would then prevent use of invalid fields, and could propagate access control through
  //a data model, etc.
  def zoom[C](lens: Lens[M, C], fieldName: String): Cursor[C] = 
    Cursor(LensParent(parent, lens, fieldName), lens.get(model))
}

object Address {
  import Street._
  
  implicit val addressDeltaReader: DeltaReader[Address] = new DeltaReader[Address] {
    def readDelta(v: Js.Value): Delta[Address] = v match {
      case Js.Obj(field, _ @ _*) => field match {
        case ("lens", v) => v match {
          case Js.Obj(field, _ @ _*) => field match {
            case ("street", v) => LensDelta(Address.street, streetDeltaReader.readDelta(v))
            case _ => throw new Invalid.Data(v, "Invalid delta, expected object with street field")
          }
        }
        case ("value", v) => ValueDelta(implicitly[Reader[Address]].read(v))
        
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

case class ValueDelta[M](v: M) extends Delta[M] {
  def apply(m: M): M = v
}

trait DeltaReader[M] {
  def readDelta(v: Js.Value): Delta[M]
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
    
    val a2 = Address.addressDeltaReader.readDelta(
      Js.Obj("lens" -> 
        Js.Obj("street" ->
          Js.Obj("lens" -> 
            Js.Obj("name" -> Js.Str("Specified new street name!"))
          )
        )
      )
    ).apply(a)

    val a3 = Address.addressDeltaReader.readDelta(
      Js.Obj("lens" -> 
        Js.Obj("street" ->
          Js.Obj("lens" -> 
            Js.Obj("number" -> Js.Num(42))
          )
        )
      )
    ).apply(a2)

    val valueDeltaJSON =       
      Js.Obj("lens" -> 
        Js.Obj("street" ->
          Js.Obj("value" -> implicitly[Writer[Street]].write(Street("Completely new value street", 9001))
          )
        )
      )

    val a4 = Address.addressDeltaReader.readDelta(valueDeltaJSON).apply(a3)

    val a5 = Address.addressDeltaReader.readDelta(
      Js.Obj("lens" -> 
        Js.Obj("street" ->
          Js.Obj("action" -> implicitly[Writer[StreetAction]].write(StreetActionNumberMultiple(2)))
        )
      )
    ).apply(a4)

    //Build a delta using cursor, just adding resulting new models to paragraphs
    //alongside the encoded JSON, then applying the delta read from encoded js
    //and comparing the result, to show that server-side replication of the
    //delta would work
    val callback = (delta: Delta[Address], deltaJs: Js.Value) => {
      val aDelta = delta.apply(a)
      appendPar(document.body, "After cursor delta applied directly: " + aDelta)
      appendPar(document.body, "Cursor delta encoded as: " + deltaJs)
      
      val aDeltaJs = Address.addressDeltaReader.readDelta(deltaJs).apply(a)
      appendPar(document.body, "After cursor delta applied using JSON: " + aDeltaJs)
      
      appendPar(document.body, "Equal? " + (aDelta == aDeltaJs))
      
    }
    val root = RootParent(callback)
    val addressCursor = Cursor(root, a)
    val streetCursor = addressCursor.zoom(Address.street, "street")
    
    streetCursor.set(Street("New street set using cursor", 9002))
    streetCursor.set(Street("Another new street set using cursor", 9003))
    streetCursor.act(StreetActionNumberMultiple(3))

    appendPar(document.body, "Before delta: " + a)
    appendPar(document.body, "After name delta: " + a2)
    appendPar(document.body, "After number delta: " + a3)
    appendPar(document.body, "After set delta: " + a4)
    // appendPar(document.body, "After action delta: " + a5)
    appendPar(document.body, "Example JSON delta: " + valueDeltaJSON)

    val e = Employee("bob", Company(Address(Street("bobstreet", 42))))
    
    val streetName = (Employee.company ^|-> Company.address ^|-> Address.street ^|-> Street.name).get(e)
    
    appendPar(document.body, streetName)
    
    appendPar(document.body, "Hello World")

    document.body.appendChild(view(e).render)
    
    val lv = listView[String]
    
    document.body.appendChild(div(lv.render(List("a", "b", "c"))).render)
  }
}
