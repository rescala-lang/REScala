# how to add global plugins

1. create a config file in your sbt installation like `$HOME/.sbt/1.0/plugins/config.sbt`.
2. add the following lines:

    `addSbtPlugin("com.timushev.sbt"   % "sbt-updates"                   % "0.6.4")`

    `addDependencyTreePlugin`

(you may need to check https://github.com/rtimush/sbt-updates for the latest sbt-updates version)


# useful commands

generate html dependency tree:

    sbt dependencyBrowserGraphHTML

show updateable dependencies and plugins:

    sbt "dependencyUpdates; reload plugins; dependencyUpdates; reload return"

generate a folder with all jars needed to run standalone (works only for the jvm part):

    sbt stageJars

execute the standalone jars:

    java --class-path "jvm/target/jars/*" dtn.start_epidemic_routing

download a graalvm for native executable creation:

    wget https://download.oracle.com/graalvm/22/latest/graalvm-jdk-22_linux-x64_bin.tar.gz
    tar -xvzf graalvm-jdk-22_linux-x64_bin.tar.gz
    rm graalvm-jdk-22_linux-x64_bin.tar.gz

create a config for native executable creation (one time only for each main method):

    graalvm-jdk-22.0.1+8.1/bin/java -agentlib:native-image-agent=config-output-dir=jvm/src/main/resources/META-INF/native-image/dtn.start_epdidemic_routing/generated --class-path jvm/target/jars/"*" dtn.start_epidemic_routing

create a native executable (important: re-export the jars first again):

    graalvm-jdk-22.0.1+8.1/bin/native-image --class-path jvm/target/jars/"*" dtn.start_epidemic_routing -o start_epidemic_routing --no-fallback --gc=G1 -Ob
