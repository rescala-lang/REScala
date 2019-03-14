package ersirjs

import ersir.shared._
import ersirjs.AppState.IndexState
import ersirjs.Definitions.path_main
import ersirjs.Navigation.{Mode, Next, Prev, navigationEvents}
import ersirjs.render.Index
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Body
import rescala.default._
import rescala.macros.cutOutOfUserComputation
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.{body, stringFrag}

import scala.scalajs.js.URIUtils.decodeURIComponent


class ReaderApp() {

  @cutOutOfUserComputation
  def makeBody(index: Index, manualStates: Event[AppState]): Signal[TypedTag[Body]] = {


    def pathToState(path: String): AppState = {
      val paths = List(path.substring(1).split("/"): _*)
      Log.JS.debug(s"get state for $paths")
      paths match {
        case Nil | "" :: Nil => IndexState
        case encodedId :: Nil =>
          val id = decodeURIComponent(encodedId)
          locally(id)
          IndexState
        case encodedId :: posS :: Nil =>
          val id = decodeURIComponent(encodedId)
          val pos = Integer.parseInt(posS)
          locally(id); locally(pos)
          IndexState
        case _ => IndexState
      }
    }

    def getHash: String = {
      dom.window.location.hash
    }

    //val hashChange =
    //  Events.fromCallback[HashChangeEvent](dom.window.onhashchange = _)
    //hashChange.event.observe(hc => Log.JS.debug(s"hash change event: ${hc.oldURL} -> ${hc.newURL}"))
    //
    //val hashBasedStates = hashChange.event.map(hc => pathToState(new URL(hc.newURL).hash): @unchecked)


    val targetStates: Event[AppState] = manualStates // || hashBasedStates

    val initialState = pathToState(getHash)
    val initialPos = initialState match {
      case IndexState => 1
    }

    Log.JS.debug(s"initial state: $initialState")

    navigationEvents.observe(n => Log.JS.debug(s"navigating $n"))



    val currentAppState: Signal[AppState] = targetStates.latest(initialState)


    val currentPosition: Signal[Int] = Events.foldAll(initialPos) { pos =>
      Seq(
        navigationEvents >> {
          case Prev => math.max(pos - 1, 0)
          case Next => math.min(pos + 1, Integer.MAX_VALUE)
          case Mode(_) => pos
        },
        targetStates >> {
          case _ => pos
        }
      )
    }
    currentPosition.observe(p => Log.JS.debug(s"current position is $p"))

    targetStates.observe(s => Log.JS.debug(s"state: $s"))


    navigationEvents.observe { case (ev) =>
      if (ev == Prev || ev == Next) {
        dom.window.scrollTo(0, 0)
      }
    }

    Signal.dynamic {
      currentAppState.value match {
        case IndexState => (path_main, "Emergencity RSS Reader")
      }
    }.observe { case (u, t) =>
      dom.window.document.title = t
      //val h = getHash
      //// for some reason, the leading # is not returned by getHash, when nothing follows
      //if (h != u && !(u == "#" && h == "")) {
      //  dom.window.history.pushState(null, null, u)
      //  Log.JS.debug(s"pushing history '$u' was '$h' length ${dom.window.history.length}")
      //}
    }

    val indexBody = index.gen()


    val bodyElement = {
      // printing the html.Body actually causes it to no longer display the inner rendered signals
      val bodySignal: Signal[TypedTag[html.Body]] = currentAppState.map {
        case IndexState => indexBody
      }
      val bodyTag: Signal[TypedTag[html.Body]] = bodySignal
        .withDefault(body("loading more data"))
        .recover { case t => body(t.toString) }
      bodyTag
    }

    bodyElement
  }

}
