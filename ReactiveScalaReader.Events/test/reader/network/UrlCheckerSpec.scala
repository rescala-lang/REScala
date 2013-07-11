package reader.network

import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import reader.EventShouldFireWrapper.convertToEventShouldFireWrapper

@RunWith(classOf[JUnitRunner])
class UrlCheckerSpec extends FunSpec with ShouldMatchers with BeforeAndAfter {
  var checker: UrlChecker = _
  
  before {
    checker = new UrlChecker
  }
  
  describe("The url checker") {
    it("should accept valid urls") {
      checker.urlIsValid shouldFireIn {
        checker.check("http://feeds.rssboard.org/rssboard")
      }
    }
    
    it("should not accept invalid urls") {
      checker.urlIsInvalid shouldFireIn {
        checker.check("garbage")
      }
    }
  }
}
