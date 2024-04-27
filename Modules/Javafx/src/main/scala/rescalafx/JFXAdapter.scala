package rescalafx

import reactives.default.*
import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, DoubleProperty, Property, StringProperty}

object JFXAdapter {

  implicit class PropertyToSignal[T, J](p: Property[T, J]) {

    def toSignal: Signal[T] = {
      val v = Var(p.value)
      p.onChange { (_, _, _) => v.set(p.value) }
      v
    }
  }

  implicit class SignalToStringProperty[T](s: Signal[T])(implicit ev: T =:= String) {
    def toProperty: StringProperty = {
      val p = StringProperty(ev(s.now))
      s observe { v =>
        Platform.runLater(p.update(ev(v)))
      }
      p
    }
  }

  implicit class SignalToDoubleProperty[T](s: Signal[T])(implicit ev: T =:= Double) {
    def toProperty: DoubleProperty = {
      val p = DoubleProperty(ev(s.now))
      s observe { v =>
        Platform.runLater(p.update(ev(v)))
      }
      p
    }
  }

  implicit class SignalToBooleanProperty[T](s: Signal[T])(implicit ev: T =:= Boolean) {
    def toProperty: BooleanProperty = {
      val p = BooleanProperty(ev(s.now))
      s observe { v =>
        Platform.runLater(p.update(ev(v)))
      }
      p
    }
  }

}
