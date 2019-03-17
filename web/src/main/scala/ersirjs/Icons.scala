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
}
