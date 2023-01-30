# SafeFBDC Programmable Architecture Example Application

As discussed in the HAP1 architecture document, there are multiple possible communication architectures viable for data safe data processing. Which architecture to use, depends on the chosen organization structure and operational constraints.

This case study demonstrates how the REScala project and replicated data types can be used to design an interactive application that makes use of one or more services within an arbitrary ad-hoc network. The services available for demonstration are deliberately simple as the focus is on the flexible design of the network communication.

Specifically, the supported services allow:
* Executing a remote command and displaying its result.
  Concretely, we use the `fortunes` command, which displays a random quote.
* Querying remote database. The database used is SQLite, we do not remotely connect to the database, but rather use our infrastructure to manage the request and execute it locally.

More exciting is the network communication part. The application itself is available for the Web and as a command line executable to be run on laptops or servers. The command line application is able to accept and create connections via TCP and websockets, while the Web application may only use websockets. Instances of the application can form arbitrary network topologies, such as client/server, full mesh, trees, or ring routing. The instances will automatically coordinate to ensure that each instance has a consistent and up-to-date state. Consistency between all instances is guaranteed, as long as the instances are at all possible to communicate (over arbitrary paths).

Within this network, each command line client automatically provides one or both of the above services if the necessary command or database file are found. Each Web client visualizes which services are available where and allow executing new tasks. For demonstration purposes, all Web clients show the same results demonstrating collaborative use.

## Installation

* Install sbt https://www.scala-sbt.org/
* Install a JDK (~17)
* Optionally, install the `fortune` binary (e.g., using `apt install fortune-mod`)
* Optionally, download the `northwind.db` [here](https://github.com/jpwhite3/northwind-SQLite3) and place it in this folder.
* Run the included `launch-server.sh` which uses sbt to compile the application and the starts an instance of the commandline application.
* Optionally, launch more instances of the commandline client using `java --class-path "jvm/target/jars/*" replication.cli conn --tcp-connect 127.0.0.1:3005` (adapt accordingly).
  Use `java --class-path "jvm/target/jars/*" replication.cli conn --help` to get an overview of availbale options.
* Connect to http://127.0.0.1:3004/ to access the Web application. Press the connect button to connect to the local commandline application. If any services are within the network, they will provide the `get fortune` and `query northwind` buttons.

## Notes

Only a single global result per service is considered. If multiple instances provide the same services they can be executed/queried independently, but the result they provide will override any other prior result of the same service. (Even if the UI does display the result per instance).

For demonstration purposes, routing of messages beyond direct neighbors has a very high communication delay (10 seconds). This allows starting multiple requests simultaneously, adding and removing various connections, and confirming that the end result is still consistent.

The overall infrastructure works in such a way that each client can store each step of the history. This is useful to roll back to a prior state, or to audit what happened. History states are stored in an efficient delta format, but can still be disabled if undesirable.

The application state of the shown application is very simple. However, the provided infrastructure is able to handle arbitrary “data classes” (product types).








# Console Replication

In this case study, multiple peers can modify a replicated add-wins-set. It was developed to test the behavior of our Delta CRDT implementation in unreliable networks simulated by the [CORE Emulator](http://coreemu.github.io/core/). Also, it features two variants of a checkpoint mechanism for reducing the overhead of comparing the local state of two replicas when a connection between them is established.

## Checkpointing

In general, the idea of checkpointing is that when a connection is established between two replicas, they only have to exchange their checkpoint information instead of their full state. Based on these checkpoints, the replicas can find out what parts of the state they have in common and only exchange the parts of the state that a replica is still missing.

### Central Checkpointing

In this implementation, a central node holds the authority over recording checkpoints. Since this node builds a linear history of checkpoints, the checkpoint that a replica is currently on can simply be represented by a single counter value. The minimum size of changes for creating a new checkpoint can be set by adjusting the minCheckpointSize field in src/main/scala/central/Checkpointer.

### Decentral Checkpoint

In this variant, each replica can create and publish checkpoints for its own changes. The checkpoint information of each replica is then encoded as a vector clock where each entry represents the checkpoint counter of a different replica. The minimum and maximum size of changes for creating a new checkpoint can be set by adjusting the minAtomsForCheckpoint and maxAtomsForCheckpoint fields in src/main/scala/decentral/Replica.

## CORE 9 Setup

use the docker method as described here, but you should be able to directly use the dockerfile in this folder
https://coreemu.github.io/core/install.html#dockerfile-based-install

```
# build image
sudo docker build -t core -f Dockerfile .
# start container
sudo docker run -itd --name core -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix:rw --privileged core
# enable xhost access to the root user
xhost +local:root
# launch core-gui
sudo docker exec -it core core-gui
```

## CORE Emulator Setup (old?)

First, follow the instructions [here](https://github.com/dtn7/dtn7-rs/blob/master/doc/getting-started.md) to install CORE and create a simulated network.

Compile the case study using the sbt stage command in the ConsoleReplication project and copy the output folder Code/Examples/ConsoleReplication/target/universal/stage into /tmp/shared.

In the docker instance, install java and iptables:

```bash
apt update
apt install openjdk-8-jre
apt install iptables
```

Start the simulation in the CORE GUI and open a console for each connected host by double-clicking it in the GUI. Then, in each host console, execute the following commands (substituting 10.0.0.10 for the ip address assigned to the respective host in the simulation):

```bash
sysctl -w net.ipv4.conf.eth0.route_localnet=1
iptables -t nat -I PREROUTING -p tcp -d 10.0.0.10/24 --dport 4444 -j DNAT --to-destination 127.0.0.1:4444
```

To simulate an unreliable network, double-clicking the wlan in the CORE GUI lets you configure some properties such as delay or loss%. To simulate replicas temporarily disconnecting and reconnecting, drag their hosts around in the GUI, they will lose the connection if they are dragged too far from other hosts.

## Running replication examples

see makefile

## Starting Replicas (old)

### Central

Start a checkpointer by executing in a host console:

```bash
/tmp/shared/stage/bin/central_console-replication checkpointer <listenport>
```

e.g.

```bash
/tmp/shared/stage/bin/central_console-replication checkpointer 4444
```

Start a replica by executing in a host console:

```bash
/tmp/shared/stage/bin/central_console-replication peer <id> <listenport> <connectTo>
```

e.g.

```bash
/tmp/shared/stage/bin/central_console-replication peer a 4444 10.0.0.11:4444 10.0.0.12:4444
```

### Decentral

Start a replica by executing in a host console:

```bash
/tmp/shared/stage/bin/decentral_console-replication replica <id> <listenport> <connectTo> <initSize>
```

e.g.

```bash
/tmp/shared/stage/bin/decentral_console-replication replica a 4444 10.0.0.10:4444 1000
```

initSize is used to initialize the set to contain the elements 0 until initSize

## Interacting with the CRDT

On each replica, the following commands can be used to interact with the add-wins-set:

* add n: add the number n to the set
* remove n: remove the number n from the set
* clear: remove all elements from the set
* elements: print all elements in the set
* size: print the size of the set
* exit: terminate the replica
