package kofre.protocol

import kofre.base.Lattice.merge
import kofre.time.VectorClock
import kofre.base.Lattice
import kofre.base.Id

import scala.collection.immutable.HashMap

type Secret = String

case class AEAD[S, A](cyphertext: S, metadata: A)

def decrypt[S, A](aead: AEAD[S, A], key: Secret): Option[S]      = Option.when(key == "secret")(aead.cyphertext)
def encrypt[S, A](data: S, metadata: A, key: Secret): AEAD[S, A] = AEAD(data, metadata)

type EnCRDT[S] = Set[AEAD[S, VectorClock]]

given encrdtLattice[S]: Lattice[EnCRDT[S]] with
  def merge(left: EnCRDT[S], right: EnCRDT[S]): EnCRDT[S] =
    val combined = left union right
    combined.filterNot(s => combined.exists(o => s.metadata < o.metadata))

extension [S](c: EnCRDT[S])
  def version: VectorClock = c.map(_.metadata).reduceOption(Lattice.merge[VectorClock]).getOrElse(VectorClock.zero)
  def send(data: S, key: Secret, replicaID: Id): EnCRDT[S] =
    val causality = c.version.inc(replicaID)
    Set(encrypt(data, causality, key))

  def recombine(key: Secret)(using Lattice[S]): Option[S] =
    c.flatMap(decrypt(_, key)).reduceOption(Lattice.merge[S])
