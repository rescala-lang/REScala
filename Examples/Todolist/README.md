# TODOMVC App

Run with:
~~~
$ npm install             # get js dependencies
$ sbt fastOptJS           # compile scala to js
$ firefox todomvc.html    # open todomvc    in browser
$ firefox simpletodo.html # open simpletodo in browser
~~~

Last tested using sbt 0.13.15, scala 2.11.8. Uses the libraries rescala,
rescalatags and scalajs-dom, see build.sbt for required versions.

If you try another scala version, you must update in todomvc.html and
simpletodo.html the script src to the new version.
  like this: "./target/scala-VERSION/daimpl-fastopt.js"

(See other branch 'chat' for another app using rescala-multitiere/retier.)

