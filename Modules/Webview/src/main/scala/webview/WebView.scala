package webview

import webview.CApi.*
import webview.WebView.CBDesc

import java.net.URI
import scala.collection.mutable
import scala.scalanative.runtime.{Intrinsics, fromRawPtr, toRawPtr}
import scala.scalanative.unsafe.*

class WebView private (val w: webview_t) {
  def setTitle(title: String): Unit = Zone {
    webview_set_title(w, toCString(title))
  }

  def setHtml(html: String): Unit = Zone {
    webview_set_html(w, toCString(html))
  }

  def init(js: String): Unit = Zone {
    webview_init(w, toCString(js))
  }

  def run(): Unit =
    try {
      webview_run(w)
      ()
    } finally webview_destroy(w)

  def navigate(uri: URI) = Zone { zone ?=>
    webview_navigate(w, toCString(uri.toASCIIString)(using zone))
  }

  def bind(name: String, f: String => String): CBDesc =
    WebView.callbacks.get(name).foreach(_.unbind())
    val handler = new WebView.CBDesc(name, f, w)
    WebView.callbacks += name -> handler
    Zone {
      webview_bind(w, toCString(name), WebView.bnd, fromRawPtr(Intrinsics.castObjectToRawPtr(handler)))
    }
    handler
}

object WebView {
  def apply() = new WebView(webview_create(1, null.asInstanceOf))

  // weird indirections because callbacks should not be GCed
  class CBDesc(name: String, val handler: String => String, val view: webview_t) {
    def unbind(): Unit = Zone {
      webview_unbind(view, toCString(name))
    }
  }
  val callbacks: mutable.Map[String, CBDesc] = mutable.Map.empty

  val bnd: BindCallback = { (seq: CString, req: CString, w: Ptr[Byte]) =>
    val cbdesc    = Intrinsics.castRawPtrToObject(toRawPtr(w)).asInstanceOf[CBDesc]
    val argstring = fromCString(req)
    val res       = cbdesc.handler(argstring)
    Zone {
      webview_return(cbdesc.view, seq, 0, toCString(res))
    }
  }
}
