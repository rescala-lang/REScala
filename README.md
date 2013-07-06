editor
======

This project is a basic text editor that comes in five flavors. All versions 
except the last one use a gap buffer, a specialized data structure to store the 
textual data and allows for efficient insertion and deletion at the caret 
position.

- **imperative**  
  no signals or events, only imperative code, value changes are propagated via 
  observables (Swing Reactors)
  
- **events**  
  REcala events (but no signals)  
  event stream composition is a bit complex, but this reduces the number of 
  places where the caret needs to be updated
  
- **signalsAndEventsFromImperative**  
  based on the *imperative* version but uses REScala events and signals to 
  express dependencies (e.g. line count etc.)
  
- **signalsAndEventsFromEventsOnly**  
  based on the *events* version but uses REScala signals
  
- **signal**  
  based on the *signals1* version  
  this version does not use a mutable data structure (gap buffer) to represent 
  the text, state is represented using signals and events only. There is just 
  very little imperative code left that is needed to interact with certain 
  library functions (like clipboard copy/paste)

