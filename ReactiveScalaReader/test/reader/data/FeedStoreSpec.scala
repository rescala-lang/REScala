package reader.data

import reader.common.implicits._
import reader.testHelpers._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfter

@RunWith(classOf[JUnitRunner])
class FeedStoreSpec extends FlatSpec with ShouldMatchers with BeforeAndAfter {
  var store: FeedStore = _
  val channel = new RSSChannel("Title",Some("http://test.de"),"Desc",None,None)
  val item = new RSSItem("Item title",None,"item desc",None,Some(channel))

  before {
    store = new FeedStore
  }

  "The feed store" should "be able to add channels" in {
    store.channels.isEmpty should equal(true)
    store.addChannel(channel)
    store.channels.size should equal(1)
  }

  it should "not add the same channel twice" in {
    store.channels.isEmpty should equal(true)
    store.addChannel(channel)
    store.addChannel(channel)
    store.channels.size should equal(1)
  }

  it should "return none via itemsFor if the channel was not yet added" in {
    store.addItem(item)
    store.itemsFor(channel) should equal(None)
  }

  it should "be able to return a Signal with the items of a channel" in {
    store.addChannel(channel)
    store.addItem(item)
    store.itemsFor(channel).get.size should equal(1)
  }

  it should "not add the same item of the same channel twice" in {
    store.addChannel(channel)
    store.addItem(item)
    store.addItem(item)
    store.itemsFor(channel).get.size should equal(1)
  }

  it should "not fire channels changed if the channel has already been added" in {
    shouldFire(store.channelsChanged) { store.addChannel(channel) }

    shouldNotFire(store.channelsChanged) { store.addChannel(channel) }
  }

  it should "only fire itemAdded if the item is new" in {
    store.addChannel(channel)

    shouldFire(store.itemAdded) { store.addItem(item) }

    shouldNotFire(store.itemAdded) { store.addItem(item) }
  }

}
