package reader.network

import reader.network._

import reader.testHelpers._

import org.scalatest._
import org.scalatest.matchers._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.net.URL

@RunWith(classOf[JUnitRunner])
class UrlCheckerSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {

  var checker: UrlChecker = _

  before {
    checker = new UrlChecker
  }

  describe("The url checker") {
    it("should accept valid urls") {
      shouldFire(checker.urlIsValid) {
        checker.check("http://feeds.rssboard.org/rssboard")
      }
    }

    it("should not accept invalid urls") {
      shouldFire(checker.urlIsInvalid) {
        checker.check("garbage")
      }
    }
  }

}
