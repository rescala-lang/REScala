package kofre.base

import java.util.{Base64, UUID}
import scala.util.Random

opaque type Id = String

object Id:
  given ordering: Ordering[Id]  = Ordering.String
  def predefined(s: String): Id = s
  def unwrap(id: Id): String    = id
  def zero: Id                  = ""
  val random: Random            = scala.util.Random()

  extension (s: String) def asId: Id = s

  /** Generates unique identifiers for use by CRDTs */
  def genId(): Id =
    val randomBytes = new Array[Byte](15)
    random.nextBytes(randomBytes)
    Base64.getEncoder.encodeToString(randomBytes)

type Time = Long

object Time:
  def current(): Time = System.currentTimeMillis
