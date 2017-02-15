package tests.rescala.fullmv.testutils

import rescala.fullmv.api.{ReevaluationTicket, SignalVersionList, Transaction}

class UserComputationTracker {
  var receivedInputs: Map[SignalVersionList.O, Map[Transaction, Transaction]] = Map.empty.withDefaultValue(Map.empty)
  var executedTransactionsInReverseOrder: Map[SignalVersionList.O, List[Transaction]] = Map.empty.withDefaultValue(List.empty)
  val comp: (ReevaluationTicket, Transaction) => Transaction = { case (ReevaluationTicket(txn, issuer), v_in) =>
    receivedInputs += issuer -> (receivedInputs(issuer) + (txn -> v_in))
    executedTransactionsInReverseOrder += issuer -> (txn :: executedTransactionsInReverseOrder(issuer))
    txn
  }
}
