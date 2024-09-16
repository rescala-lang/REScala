package ex2024dare

import rdts.base.{LocalUid, Uid}
import rdts.dotted.Dotted
import rdts.time.{Dot, Dots}

object ACL {

  trait Operation(id: Dot, signer: User, predecessors: Dots)

  object Operation {
    case class Create(signer: User, id: Dot, roomid: Uid)
        extends Operation(id, signer, Dots.empty)
    case class Add(signer: User, added: User, id: Dot, predecessors: Dots)
        extends Operation(id, signer, predecessors)
    case class Remove(signer: User, removed: User, id: Dot, predecessors: Dots)
        extends Operation(id, signer, predecessors)
  }

  case class User(uid: Uid)

  case class OpGraph(ops: Map[Dot, Operation]) {

    def dots: Dots = Dots.from(ops.keys)

    def create()(using context: Dots, replicaId: LocalUid): Dotted[OpGraph] = {
      val next = context.nextDot(replicaId.uid)

      val op = Operation.Create(User(replicaId.uid), next, Uid.gen())

      Dotted(OpGraph(Map(next -> op)), Dots.single(next))
    }

    def add(user: User)(using context: Dots, replicaId: LocalUid): Dotted[OpGraph] = {
      val next = context.nextDot(replicaId.uid)

      val op = Operation.Add(User(replicaId.uid), user, next, dots)

      Dotted(OpGraph(Map(next -> op)), Dots.single(next))
    }

    def remove(user: User)(using context: Dots, replicaId: LocalUid): Dotted[OpGraph] = {
      val next = context.nextDot(replicaId.uid)

      val op = Operation.Remove(User(replicaId.uid), user, next, dots)

      Dotted(OpGraph(Map(next -> op)), Dots.single(next))
    }

  }

}
