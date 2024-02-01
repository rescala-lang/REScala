package loci
package registry

import communicator.tcp._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class TCPRegistrySpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "TCP Registry"

  val port = 45850

  it should "handle binding and lookup correctly" in {
    for (_ <- 1 to 1000)
      RegistryTests.`handle binding and lookup correctly`(TCP(port), TCP("localhost", port))
  }

  it should "handle subjective binding and lookup correctly" in {
    for (_ <- 1 to 1000)
      RegistryTests.`handle subjective binding and lookup correctly`(TCP(port), TCP("localhost", port))
  }
}
