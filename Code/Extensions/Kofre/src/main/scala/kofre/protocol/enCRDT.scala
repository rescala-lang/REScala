package kofre.protocol

import kofre.Lattice.merge
import kofre.primitives.VectorClock
import kofre.{IdUtil, Lattice}

import scala.collection.immutable.HashMap

type Secret = String

case class Encrypted[S](data: S)

case class AEAD[S](cyphertext: Encrypted[S], metadata: VectorClock)
def encrypt[T](data: T, metadata: VectorClock, key: Secret): AEAD[T] = AEAD(Encrypted(data), metadata)
def decrypt[T](key: Secret)(aead: AEAD[T]) = Option.when(key == "secret")(aead.cyphertext.data)

type EnCRDT[S] = Set[AEAD[S]]

given [S]: Lattice[EnCRDT[S]] with
  def merge(left: EnCRDT[S], right: EnCRDT[S]): EnCRDT[S] =
    val combined = left union right
    combined.filterNot(s => combined.exists(o => s.metadata < o.metadata))

extension [S](c: EnCRDT[S])
  def version: VectorClock = c.map(_.metadata).reduceOption(Lattice.merge[VectorClock]).getOrElse(VectorClock.zero)
  def send(data: S, key: Secret, replicaID: IdUtil.Id): EnCRDT[S] =
    val causality = c.version.inc(replicaID)
    Set(encrypt(data, causality, key))

  def recombine(key: Secret)(using Lattice[S]): Option[S] =
    c.flatMap(decrypt(key)).reduceOption(Lattice.merge[S])
