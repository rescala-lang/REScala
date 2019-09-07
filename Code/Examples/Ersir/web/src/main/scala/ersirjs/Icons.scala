package ersirjs


import scalatags.JsDom.all.{Modifier, raw}

object Icons {
  val lamp: Modifier =raw("""
<svg viewBox="0 0 512 512" >
 <def>


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
      <path transform="translate(256, 180)" id="lamphead"
         d="m 32 0
            h -64
            l -10 -96
            h 84
            l -10 96
            z" />
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

  val disconnected = raw("""
<svg x="0px" y="0px" viewBox="0 0 1000 1000" enable-background="new 0 0 1000 1000" xml:space="preserve">
<metadata> Svg Vector Icons : http://www.onlinewebfonts.com/icon </metadata>
<g><path d="M467.6,640.9c6.1,8.5,7.2,17.5,7.2,22.6c0,6-1.5,17.6-11.2,27.4l-183,183c-9.8,9.8-21.3,11.2-27.4,11.2c-6,0-17.6-1.5-27.4-11.2L121.7,769.7c-9.8-9.8-11.2-21.3-11.2-27.4c0-6.1,1.5-17.6,11.2-27.4l183-183c9.8-9.8,21.3-11.2,27.4-11.2c4.3,0,11.2,0.8,18.3,4.6l55.6-55.6c-45.5-37.6-113.5-35-156,7.5l-183,183c-45.2,45.2-45.2,119.1,0,164.3L171,928.5c45.2,45.2,119.1,45.2,164.3,0l183-183c43.7-43.7,45.1-114.2,4.4-159.7L467.6,640.9z M928.6,171L824.5,66.9c-45.2-45.2-119.1-45.2-164.3,0l-183,183c-42.5,42.5-45,110.5-7.5,156l55.6-55.6c-3.8-7.2-4.6-14.1-4.6-18.3c0-6,1.5-17.6,11.2-27.4l183-183c9.8-9.8,21.3-11.2,27.4-11.2c6,0,17.6,1.5,27.4,11.2l104.1,104.1c9.8,9.8,11.2,21.3,11.2,27.4c0,6-1.5,17.6-11.2,27.4l-183,183c-9.8,9.8-21.3,11.2-27.4,11.2c-5,0-14-1.1-22.6-7.2l-55,55c45.5,40.8,116,39.4,159.7-4.4l183-183C973.8,290,973.8,216.1,928.6,171z M318.7,252.9c18.2,18.2,18.3,47.5,0,65.7c-18.1,18.1-47.7,18-65.7,0l-92-92c-18.2-18.2-18.3-47.5,0-65.7c18.1-18.1,47.7-18,65.7,0L318.7,252.9z M404.6,186.5c0,25.7-20.6,46.5-46.5,46.5c-25.7,0-46.5-21-46.5-46.5V56.4c0-25.7,20.6-46.5,46.5-46.5c25.7,0,46.5,21,46.5,46.5V186.5z M186.6,311.6c25.7,0,46.5,20.6,46.5,46.5c0,25.7-21,46.5-46.5,46.5H56.5C30.8,404.5,10,383.9,10,358c0-25.7,21-46.5,46.5-46.5H186.6z M681.3,747.1c-18.2-18.2-18.3-47.5,0-65.7c18.1-18.1,47.7-18,65.7,0l92,92c18.2,18.2,18.3,47.5,0,65.7c-18.1,18.1-47.7,18-65.7,0L681.3,747.1z M595.4,813.5c0-25.7,20.6-46.5,46.5-46.5c25.7,0,46.5,21,46.5,46.5v130.1c0,25.7-20.6,46.5-46.5,46.5c-25.7,0-46.5-21-46.5-46.5V813.5z M813.4,688.4c-25.7,0-46.5-20.6-46.5-46.5c0-25.7,21-46.5,46.5-46.5h130.1c25.7,0,46.5,20.6,46.5,46.5c0,25.7-21,46.5-46.5,46.5H813.4z"/></g>
</svg>""")
}
