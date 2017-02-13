package tests.rescala.fullmv.testutils

import rescala.fullmv.api.Transaction

/**
  * Created by MisterD on 13.02.2017.
  */
class UserComputationTracker {
  var receivedInputs: Map[Transaction, Transaction] = Map.empty
  var executedTransactionsInOrder: Seq[Transaction] = Seq.empty
  val comp: (Transaction, Transaction) => Transaction = {(txn: Transaction, v_in: Transaction) =>
    receivedInputs += txn -> v_in
    txn
  }
}
