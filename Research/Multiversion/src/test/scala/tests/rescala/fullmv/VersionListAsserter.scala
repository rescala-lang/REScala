//package tests.rescala.fullmv
//
//import org.scalatest.Matchers
//import rescala.fullmv.api.{SignalVersionList, Version}
//
//import scala.annotation.tailrec
//
//object VersionListAsserter extends Matchers {
//  def assertVersions(node: SignalVersionList[_], versions: Version[_]*): Unit = {
//    @tailrec def equalPrefix(test: Int): Int = {
//      if(test == versions.size || test == node._versions.size || !versionsEqual(versions(test), node._versions(test))) {
//        test
//      } else {
//        equalPrefix(test + 1)
//      }
//    }
//    val prefix = equalPrefix(0)
//    if(prefix == versions.size) {
//      assert(prefix == node._versions.size, "Node contains superfluous versions:\n\t" + node._versions.drop(prefix).mkString("\n\t"))
//    } else {
//      @tailrec def equalSuffix(test: Int): Int = {
//        if(test == versions.size || test == node._versions.size || !versionsEqual(versions(versions.size - 1 - test), node._versions(node._versions.size - 1 - test))) {
//          test
//        } else {
//          equalSuffix(test + 1)
//        }
//      }
//      val suffix = equalSuffix(0)
//      fail("Node versions mismatch (ignoring equal prefix of "+prefix+" and suffix of "+suffix+")!" +
//        "\n\tExpected:\n\t\t" + versions.drop(prefix).take(versions.size - suffix - prefix).mkString("\n\t\t") +
//        "\n\tActual:\n\t\t" + node._versions.drop(prefix).take(node._versions.size - suffix - prefix).mkString("\n\t\t"))
//    }
//  }
//
//  def versionsEqual(one: Version[_], two: Version[_]): Boolean = {
//    one.txn == two.txn && one.out == two.out && one.pending == two.pending && one.changed == two.changed && one.value == two.value
//  }
//}
