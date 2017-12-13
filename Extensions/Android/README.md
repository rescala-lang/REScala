# RESensors
RESensor is Rescala's Extension for Android Sensors.

# Prerequisites for Usage
Install Scala 2.11., Android SDK (at least v25.0.0) and SBT. See [here](http://scala-android.org/ide_integration/) for JetBrains IntelliJ.


# Usage in Android-project
#### Create Project and Run Configuration  ####
Create new SBT-project, set Android SDK as Project SDK. Add new Run Configuration as *Android Application*, delete all Tasks and create an SBT Task. As parameters for this SBT Task add *++2.11.11 android:package*. During development IntelliJ's SBT-Plugin did not operate correctly, we therefore installed SBT and used it as *External Tool*.

#### Usage and Development ####
For an example of API usage see the **Barometer4Android** project in 'Examples/'.

For Development either test with your own Android-Project (follow instructions above) or the existing project. Then take a look at the **ReAndroidThings** project in 'Extensions/'. 'src/main/scala/reandroidthings/' contains all objects relevant for further development.
