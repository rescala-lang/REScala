# REScala • [TodoMVC](http://todomvc.com)

>
> REScala is a Scala library for functional reactive programming on the JVM and
> the Web. It provides a rich API for event stream transformations and signal
> composition with managed consistent up-to-date state and minimal syntactic
> overhead. It supports concurrent and distributed programs.
>
> REScala — www.rescala-lang.com
>

## Implementation

We implemented most of the TodoMVC specification.
Especially interesting is the use of reactive signals to generate html-elements,
which update automatically with the sync, and the ability to persist state into
localStorage automatically, by saving all signals values.

 *  New todos are entered in top input, which is autofocused.
    Enter clears input, input is trimmed, and empty todos are not allowed.
 *  Hide footer when there are no todos.
 *  Mark-all button makes toggles every todos done state.
 *  Clicking todo checkbox toggles their done state.
 *  Double clicking a todo label turns it into editing mode.
    Editing hides checkbox and deletion button.
    blur and enter end editing.
 *  On changes, input is trimmed. Empty todos shall be deleted.
 *  Deletion button is shown on hovering over todos.
 *  Todo counter is pluralized correctly: 0 item**s**, 1 item, 2 item**s**, ... .
 *  The clear-completed button is only visible when there are todos.
 *  Todo state persists using localStorage over consecutive page visits.

We have not implemented the following things:

 *  To integrate seamlessly into scala development, we use sbt for dependency
    management instead of npm.
 *  If all todos get marked done, the Mark-done button should show this.
 *  Double clicking focused the editing input.
 *  We have not implemented routing and the related todo filters.
 *  Escaping the input shall cancel editing, restore previous value.

Tested with Firefox 61 and Chrome 66.

## Running

You need to install npm and sbt beforehand. Last tested using sbt 0.13.15.
Sbt will then download the correct versions of scala, rescala, rescalatags and
scalajs-dom, see build.sbt for required versions.

Run with:

~~~
cd "REScala main repository"
$ sbt "project todolist" deploy           # get scala dependencies and compile scala to js
cd "todolist subfolder (this file)"
$ firefox target/index.html               # open todomvc in browser
~~~




