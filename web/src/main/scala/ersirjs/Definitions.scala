package ersirjs


import scalatags.JsDom.all.{Frag, Modifier, SeqFrag, Tag, a, cls, href, raw, stringAttr}


object Definitions {

  def path_main = "#"
  def path_css = "css"
  def path_search = "s"
  def path_stop = "stop"
  def path_tools = "tools"

  val class_post = cls := "post"
  val class_extern = cls := "extern"
  val class_placeholder = cls := "placeholder"
  val class_dead = cls := "dead"
  val class_preview = cls := "preview"
  val class_chapters = cls := "chapters"
  val class_button = cls := "pure-button"
  val class_button_disabled = cls := "pure-button pure-button-disabled"
  val class_button_active = cls := "pure-button pure-button-active"
  val class_button_group = cls := "pure-button-group"


  def link_tools(ts: Frag*): Tag = a(class_button, href := path_tools)(ts)

  private def getDefined[T](ts: T*): Option[T] = ts.find(v => v != null && !scalajs.js.isUndefined(v))
  private val dDocument = scala.scalajs.js.Dynamic.global.document


  def isFullscreen(): Boolean =
    getDefined(dDocument.fullscreenElement,
               dDocument.webkitFullscreenElement,
               dDocument.mozFullScreenElement,
               dDocument.msFullscreenElement).isDefined

  def toggleFullscreen(): Unit = {
    val de = dDocument.documentElement


    def requestFullscreen = getDefined(
      de.requestFullscreen,
      de.msRequestFullscreen,
      de.mozRequestFullScreen,
      de.webkitRequestFullscreen)

    def exitFullscreen = getDefined(
      dDocument.exitFullscreen,
      dDocument.webkitExitFullscreen,
      dDocument.mozCancelFullScreen,
      dDocument.msExitFullscreen)

    if (isFullscreen()) exitFullscreen.foreach(_.call(dDocument)) else requestFullscreen.foreach(_.call(de))
  }
}

object Icons {
  val modus: Modifier = raw("""<svg aria-hidden="true" data-prefix="far" data-icon="window-maximize" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" class="svg-inline--fa fa-window-maximize fa-w-16"><path fill="currentColor" d="M464 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h416c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zm0 394c0 3.3-2.7 6-6 6H54c-3.3 0-6-2.7-6-6V192h416v234z" class=""></path></svg>""")
  val bookmark: Modifier = raw("""<svg aria-hidden="true" data-prefix="far" data-icon="bookmark" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512" class="svg-inline--fa fa-bookmark fa-w-12"><path fill="currentColor" d="M336 0H48C21.49 0 0 21.49 0 48v464l192-112 192 112V48c0-26.51-21.49-48-48-48zm0 428.43l-144-84-144 84V54a6 6 0 0 1 6-6h276c3.314 0 6 2.683 6 5.996V428.43z" class=""></path></svg>""")
  val maximize: Modifier = raw("""<svg aria-hidden="true" data-prefix="fas" data-icon="expand-arrows-alt" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512" class="svg-inline--fa fa-expand-arrows-alt fa-w-14"><path fill="currentColor" d="M448.1 344v112c0 13.3-10.7 24-24 24h-112c-21.4 0-32.1-25.9-17-41l36.2-36.2L224 295.6 116.8 402.9 153 439c15.1 15.1 4.4 41-17 41H24c-13.3 0-24-10.7-24-24V344c0-21.4 25.9-32.1 41-17l36.2 36.2L184.5 256 77.2 148.7 41 185c-15.1 15.1-41 4.4-41-17V56c0-13.3 10.7-24 24-24h112c21.4 0 32.1 25.9 17 41l-36.2 36.2L224 216.4l107.3-107.3L295.1 73c-15.1-15.1-4.4-41 17-41h112c13.3 0 24 10.7 24 24v112c0 21.4-25.9 32.1-41 17l-36.2-36.2L263.6 256l107.3 107.3 36.2-36.2c15.1-15.2 41-4.5 41 16.9z" class=""></path></svg>""")
  val externalLink: Modifier = raw("""<svg aria-hidden="true" data-prefix="fas" data-icon="external-link-alt" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 576 512" class="svg-inline--fa fa-external-link-alt fa-w-18"><path fill="currentColor" d="M576 24v127.984c0 21.461-25.96 31.98-40.971 16.971l-35.707-35.709-243.523 243.523c-9.373 9.373-24.568 9.373-33.941 0l-22.627-22.627c-9.373-9.373-9.373-24.569 0-33.941L442.756 76.676l-35.703-35.705C391.982 25.9 402.656 0 424.024 0H552c13.255 0 24 10.745 24 24zM407.029 270.794l-16 16A23.999 23.999 0 0 0 384 303.765V448H64V128h264a24.003 24.003 0 0 0 16.97-7.029l16-16C376.089 89.851 365.381 64 344 64H48C21.49 64 0 85.49 0 112v352c0 26.51 21.49 48 48 48h352c26.51 0 48-21.49 48-48V287.764c0-21.382-25.852-32.09-40.971-16.97z" class=""></path></svg>""")
  val front: Modifier = raw("""<svg aria-hidden="true" data-prefix="far" data-icon="list-alt" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" class="svg-inline--fa fa-list-alt fa-w-16"><path fill="currentColor" d="M464 32H48C21.49 32 0 53.49 0 80v352c0 26.51 21.49 48 48 48h416c26.51 0 48-21.49 48-48V80c0-26.51-21.49-48-48-48zm-6 400H54a6 6 0 0 1-6-6V86a6 6 0 0 1 6-6h404a6 6 0 0 1 6 6v340a6 6 0 0 1-6 6zm-42-92v24c0 6.627-5.373 12-12 12H204c-6.627 0-12-5.373-12-12v-24c0-6.627 5.373-12 12-12h200c6.627 0 12 5.373 12 12zm0-96v24c0 6.627-5.373 12-12 12H204c-6.627 0-12-5.373-12-12v-24c0-6.627 5.373-12 12-12h200c6.627 0 12 5.373 12 12zm0-96v24c0 6.627-5.373 12-12 12H204c-6.627 0-12-5.373-12-12v-24c0-6.627 5.373-12 12-12h200c6.627 0 12 5.373 12 12zm-252 12c0 19.882-16.118 36-36 36s-36-16.118-36-36 16.118-36 36-36 36 16.118 36 36zm0 96c0 19.882-16.118 36-36 36s-36-16.118-36-36 16.118-36 36-36 36 16.118 36 36zm0 96c0 19.882-16.118 36-36 36s-36-16.118-36-36 16.118-36 36-36 36 16.118 36 36z" class=""></path></svg>""")
  val prev: Modifier = raw("""<svg aria-hidden="true" data-prefix="far" data-icon="caret-square-left" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512" class="svg-inline--fa fa-caret-square-left fa-w-14"><path fill="currentColor" d="M272 157.1v197.8c0 10.7-13 16.1-20.5 8.5l-98.3-98.9c-4.7-4.7-4.7-12.2 0-16.9l98.3-98.9c7.5-7.7 20.5-2.3 20.5 8.4zM448 80v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48zm-48 346V86c0-3.3-2.7-6-6-6H54c-3.3 0-6 2.7-6 6v340c0 3.3 2.7 6 6 6h340c3.3 0 6-2.7 6-6z" class=""></path></svg>""")
  val next: Modifier = raw("""<svg aria-hidden="true" data-prefix="far" data-icon="caret-square-right" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512" class="svg-inline--fa fa-caret-square-right fa-w-14"><path fill="currentColor" d="M176 354.9V157.1c0-10.7 13-16.1 20.5-8.5l98.3 98.9c4.7 4.7 4.7 12.2 0 16.9l-98.3 98.9c-7.5 7.7-20.5 2.3-20.5-8.4zM448 80v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48zm-48 346V86c0-3.3-2.7-6-6-6H54c-3.3 0-6 2.7-6 6v340c0 3.3 2.7 6 6 6h340c3.3 0 6-2.7 6-6z" class=""></path></svg>""")
  val archive: Modifier = raw("""<svg aria-hidden="true" data-prefix="fas" data-icon="unlink" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" class="svg-inline--fa fa-unlink fa-w-16"><path fill="currentColor" d="M304.083 405.907c4.686 4.686 4.686 12.284 0 16.971l-44.674 44.674c-59.263 59.262-155.693 59.266-214.961 0-59.264-59.265-59.264-155.696 0-214.96l44.675-44.675c4.686-4.686 12.284-4.686 16.971 0l39.598 39.598c4.686 4.686 4.686 12.284 0 16.971l-44.675 44.674c-28.072 28.073-28.072 73.75 0 101.823 28.072 28.072 73.75 28.073 101.824 0l44.674-44.674c4.686-4.686 12.284-4.686 16.971 0l39.597 39.598zm-56.568-260.216c4.686 4.686 12.284 4.686 16.971 0l44.674-44.674c28.072-28.075 73.75-28.073 101.824 0 28.072 28.073 28.072 73.75 0 101.823l-44.675 44.674c-4.686 4.686-4.686 12.284 0 16.971l39.598 39.598c4.686 4.686 12.284 4.686 16.971 0l44.675-44.675c59.265-59.265 59.265-155.695 0-214.96-59.266-59.264-155.695-59.264-214.961 0l-44.674 44.674c-4.686 4.686-4.686 12.284 0 16.971l39.597 39.598zm234.828 359.28l22.627-22.627c9.373-9.373 9.373-24.569 0-33.941L63.598 7.029c-9.373-9.373-24.569-9.373-33.941 0L7.029 29.657c-9.373 9.373-9.373 24.569 0 33.941l441.373 441.373c9.373 9.372 24.569 9.372 33.941 0z" class=""></path></svg>""")

  val lamp: Modifier =raw("""<svg viewBox="0 0 512 512" >
 <def>
   <path id="lamphead"
         d="m 32 0
            h -64
            l -10 -96
            h 84
            l -10 96
            z" />

   <path id="lamphat"
         d="m 64 0
            h -128
            v -16
            c 0 -16 , 32 -32 ,  48 -32
            v -8
            h 32
            v 8
            c 16 0 , 48 16 , 48 32
            v 16
            z"  />

   <rect id="lampmast"
         x="-16"
         width="32"
         height="220"
         />

   <polygon id="lampsocket"
            points="
                    -24 0,
                    24 0,
                    36 80,
                    50 80,
                    50 107,
                    -50 107,
                    -50 80,
                    -36 80 "
            />
   <path id="wave"
         style="stroke-linecap:round"
         d="m 0 0 q -32 64 , 0 128"
         />

   <g id = "waves">
     <use x="210" y="107" xlink:href="#wave"
          style="stroke-width:11"
          transform="rotate(0 0 0)
                     scale(0.8 0.8)"/>
     <use x="138" y="74" xlink:href="#wave" style="stroke-width:10"/>
     <use x="90" y="50" xlink:href="#wave"
          style="stroke-width:9"
          transform="rotate(0 0 0)
                     scale(1.2 1.2)"/>
   </g>
 </def>

 <g style="fill:none;stroke:#444041;stroke-width:10">
   <use x="256" y="180" xlink:href="#lamphead"/>
   <use x="256" y="84" xlink:href="#lamphat" />
   <use x="256" y="180" xlink:href="#lampmast" />
   <use x="256" y="400" xlink:href="#lampsocket" />
   <g class="waves" style = "transform-origin: 50% 30%;">
     <use xlink:href="#waves" />
     <use xlink:href="#waves" x = "-512"
          transform="scale(-1, 1)"/>
     <animateTransform id="animbigger" attributeName="transform"
                       begin = "0s; animsmaller.end"
                       type="scale" dur="1s" from="1 1"
                       to="1.1 1" fill="freeze" />
     <animateTransform id="animsmaller" attributeName="transform"
                       begin="animbigger.end"
                       type="scale" dur="1s" from="1.1 1"
                       to="1 1" fill="freeze" />
   </g>
 </g>
</svg>
""")
}
