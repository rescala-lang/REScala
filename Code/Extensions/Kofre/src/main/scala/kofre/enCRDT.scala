package kofre

import scala.collection.immutable.HashMap
import kofre.Lattice.merge

type Secret = String

case class Encrypted[S](data: S)

case class AEAD[S](cyphertext: Encrypted[S], metadata: Version)
def encrypt[T](data: T, metadata: Version, key: Secret): AEAD[T] = AEAD(Encrypted(data), metadata)
def decrypt[T](key: Secret)(aead: AEAD[T])                       = Option.when(key == "secret")(aead.cyphertext.data)

type EnCRDT[S] = Set[AEAD[S]]

given [S]: Lattice[EnCRDT[S]] with
  def merge(left: EnCRDT[S], right: EnCRDT[S]): EnCRDT[S] =
    val combined = left union right
    combined.filterNot(s => combined.exists(o => s.metadata < o.metadata))

extension [S](c: EnCRDT[S])
  def version: Version = c.map(_.metadata).reduceOption(Lattice.merge[Version]).getOrElse(Version.zero)
  def send(data: S, key: Secret, replicaID: ReplicaID): EnCRDT[S] =
    val causality = c.version.inc(replicaID)
    Set(encrypt(data, causality, key))

  def recombine(key: Secret)(using Lattice[S]): Option[S] =
    c.flatMap(decrypt(key)).reduceOption(Lattice.merge[S])

@main
def test2() =
  val key  = "secret"
  val ec   = Set(): EnCRDT[Map[String, Int]]
  val res  = ec.send(Map("Hello" -> 5), key, "a")
  val resb = res merge res.send(Map("Hello" -> 10), key, "b")
  val res2 = res merge res.send(Map("Hello" -> 4), key, "a")
  println(res2 merge resb)
  println(res2.merge(resb).recombine(key))
