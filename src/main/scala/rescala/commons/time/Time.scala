package rescala.commons.time

import scala.language.implicitConversions

class Time(val ns: Long) extends AnyVal {
  def mus: Long = ns / 1000
  def ms: Long = mus / 1000
  def s: Double = ms / 1000.0

  def +(t: Time) = new Time(ns + t.ns)
  def -(t: Time) = new Time(ns - t.ns)
  def *(i: Int) = new Time(ns * i)
  def *(d: Double) = new Time((ns * d).toLong)
  def *(t: Time) = new Time(ns * t.ns)
  def /(i: Int) = new Time(ns / i)
  def /(d: Double) = new Time((ns / d).toLong)
  def /(t: Time) = ns.toDouble / t.ns
  def %(i: Int) = new Time(ns % i)
  def %(d: Double) = new Time((ns % d).toLong)
  def %(t: Time) = new Time(ns % t.ns)

  def since(t: Time) = this - t
  def to(t: Time) = t - this

  override def toString = ns + "ns"
}

object Time {
  def current = new Time(System.nanoTime)
  def ns(nanos: Long) = new Time(nanos)
  def mus(micros: Long) = ns(1000 * micros)
  def ms(millis: Long) = mus(1000 * millis)
  def s(s: Double) = ms((1000 * s).toLong)
  
  /** Default conversion converts from int converts as milliseconds */
  implicit def fromMs(millis: Int): Time = ms(millis)
  
  /** Default conversion from double converts as seconds */
  implicit def fromS(seconds: Double): Time = s(seconds)
  
  /** Default conversion to double converts to seconds */
  implicit def fromTime(time: Time): Double = time.s
}
  

class TimePostfixedValue(val l: Double) extends AnyVal {
    def s = new Time((1000000000 * l).toLong)
    def ms = new Time(1000000 * l.toLong)
    def mus = new Time(1000 * l.toLong)
    def ns = new Time(l.toLong)
}


object TimePostfixedValue {
  implicit def fromDouble(d: Double): TimePostfixedValue = new TimePostfixedValue(d)
  implicit def fromInt(i: Int): TimePostfixedValue = new TimePostfixedValue(i)
  implicit def fromLong(l: Long): TimePostfixedValue = new TimePostfixedValue(l)
}
