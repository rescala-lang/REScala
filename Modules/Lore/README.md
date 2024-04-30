# LoRe
> Local-first reactive programming with verified safety guarantees 🌈

This repository contains our prototype implementation of the LoRe language compiler as described [in this paper](https://doi.org/10.1145/3633769).


# Artifact Getting Started Guide

Currently, the easiest way to try out LoRe is through the ECOOP23 artifact which is available here: [https://doi.org/10.4230/DARTS.9.2.11](https://doi.org/10.4230/DARTS.9.2.11)

The goal of this artifact is to evaluate the performance of the verification process used by the LoRe compiler.

## Contents of the artifact:

- ***lore.docker.tar:*** An executable docker image of LoRe's verification backend that allows compiling LoRe programs to Viper intermediate language.
- ***viper.docker.tar:*** An executable docker image that contains the [Viper](https://www.pm.inf.ethz.ch/research/viper.html) verifier and utilities for benchmarking.
- ***examples/:*** The LoRe source code of two example applications: The calendar application from the paper and the TPC-C benchmark.
- ***sources/:*** The source code of LoRe's verification backend (written in Scala).

## Prequisites
In order to run the supplied Docker images, they have to be imported first. This can be done via the following commands (with Docker).

```shell
$ docker load -i viper.docker.tar
$ docker load -i lore.docker.tar
```

Note: It should be possible to use an alternative container runtime such as [Podman](https://podman.io), however, this tutorial will show the Docker commands. Depending on your Docker installation, you might have to prefix the commands with `sudo`.

## Reproducing the Evaluation (for the Functional Badge)

This step will reproduce the verification performance evaluation described in Section 5.2 of the paper.

First, compile the benchmark case-studies (written in LoRe language) with:

```shell
$ docker run --rm --volume "$PWD":/pwd --entrypoint /compile_benchmarks localhost/lore examples/benchmarks
```

Explanation:
- `--rm` deletes the container after it completes
- `--volume "$PWD":/pwd` mounts the current working directory as a volume inside the container. This enables the docker container to access the files in this directory
- `--entrypoint /compile_benchmarks` specifies that we want to batch compile a folder with benchmark applications instead of using the LoRe CLI normally.

This command should have created and populated the folder `examples/benchmarks_compiled`.
Afterwards, we can benchmark the verifier with:

```shell
$ docker run --rm -it --volume "$PWD":/pwd --env BENCHMARK=true --env RUNS=5 localhost/viper --dir examples/benchmarks_compiled/
```

Depending on your machine, this command will take 10 to 30 minutes to complete. (progress bars with ETAs for each verification should be present)

Explanation:
- `-it` starts an interactive terminal session. This allows us to observe the process
- `--env BENCHMARK=true` specif
ies that we want to measure and benchmark the verifier performance instead of using the verifier CLI
- `--env RUNS=5` let's us control how many verification runs per file are performed. After all runs the average time to complete is calculated. Increasing this number will lead to more precise results but will take longer.
- `--dir` tells the verifier to verify all the contents of a directory instead of a single file.


**Inspecting the results:**

The benchmarks will create a directory `tmp/` in which you should find a `benchmark.md` file containing the timing measurements. The results are presented as a markdown table and should be comparable to Table 1 in the paper.
As with all SMT-based solutions, the verification times will sometimes vary from run to run and will also differ depending on the machine. However, the verification of all benchmarks should complete successfully and verification times should be somewhere between ~5 seconds and up to a few minutes with the TPC-C benchmarks taking more time than the calendar application.

(Disclaimer: During the preparation of the artifact, we fixed a bug in our implementation of the TPC-C payment transaction. Using the fixed implementation, payment no longer overlaps with any of the remaining consistency conditions and is therefore not part of the benchmark.)


**Further Benchmarking Options:**

Previous versions of this benchmark used the [ViperServer](https://github.com/viperproject/viperserver) utility to account for JVM startup time. ViperServer makes it possible to submit verification requests to an already running Viper verification backend interactively. Using ViperServer, the benchmark process is the following: 1) start ViperServer, 2) sleep for 10s to let the JVM start up, 3) submit the verification request to ViperServer and measure the time, 4) kill the ViperServer jvm, 5) start over.

Unfortunately, the benchmark results with ViperServer turned out to be rather flaky depending on the host machine. As a result, the benchmarks now use Vipers [Silicon](https://github.com/viperproject/silicon) backend directly.

This default process can be modified via environment variables in the following way:

- `--env SHOW_OUTPUT=true` when in benchmark mode, this displays the output of the verification commands instead of displaying the progress bar.
- `--env BENCHMARK=false` disables benchmarking mode. Instead of executing the verification multiple times and measuring the time with [hyperfine](https://github.com/sharkdp/hyperfine), the verification backend will be invoced directly. This is useful if the automated benchmarks failed and one wants to inspect the command output. In this mode, no results table (see next section) will be generated, however, both ViperServer and Silicon do report the verification times after a successful verification.
- `--env VIPERSERVER=true` will use the client/server procedure that was described in the previous paragraph. *Warning: In our experience, this is less reliable than using Silicon directly!*
- `--env SLEEP_BEFORE=10s` and `--env SLEEP_AFTER=5s`, when using ViperServer, these variables control how long we wait before starting the verification and after killing the server.

The benchmarking script (written for fish shell) can be inspected in the sources folder at `sources/benchmark.fish`.

## Modifying a Case Study (for the Reusable Badge)

This section describes how one can reuse this artifact to write and verify custom LoRe programs.

### Modifying the Calendar Case study

The file `examples/calendar/calendar.lore` contains the sourcecode of the calendar example from Listing 1 in the paper.

We can use the LoRe compiler to compile this application to Viper using the following command:

```sh
$ docker run --rm --volume "$PWD":/pwd localhost/lore viperize examples/calendar/calendar.lore -o examples/calendar/calendar.vpr
```

You can inspect the generated `calendar.vpr` to see the generated intermediary code, which is presented in extracts in Listing 2 and Listing 3 in the paper.

Afterwards, we can verify the generated code using:

```sh
$ docker run --rm --volume "$PWD":/pwd localhost/viper examples/calendar/calendar.vpr
```

**Introducing a simple change:**
In order to examine the effect of a code change on the verification results, we can now change the `examples/calendar.lore` file.
A simple change would be to comment out (with a `//`) the precondition of `add_vacation` in line 16.
After compilation with LoRe and Verification with Viper, we should see the following error reported by Silicon (Viper's symbolic execution backend):

```
[0] Postcondition of add_vacation might not hold. Assertion 30 - sumDays((toSet(graph.vacation): Set[Appointment])) >= 0 might not hold. (calendar.vpr@157.9--157.30)
```

Closing remarks:
One can try introducing further changes to the calendar application and see how this affects the verification result.
Whenever these changes introduce new function calls in the source code, these have to be specified in the file `examples/calendar/deps/calendar_header.vpr`.
When working with `.vpr` files, we recommend using the *[Viper IDE](https://www.pm.inf.ethz.ch/research/viper/downloads.html)* extension for Visual Studio Code.
When working with `.lore` files, we recommend using Scala syntax highlighting as most of LoRe's syntax is valid Scala syntax.

### Modifying the compiler

We include the source code of the LoRe compiler in the `sources` folder. It is a Scala project using the sbt build tool.

We can enter an interactive sbt session using docker by doing:
```shell
$ cd sources
$ docker run --rm --volume "$PWD":/pwd --entrypoint sbt -it localhost/lore
```

Inside this sbt session, we can, e.g., run tests using `test` or compile the project to a fat jar using `assembly`.
This jar will reside under `target/scala-3.2.1/lore-assembly-0.1.0-SNAPSHOT` and can be used as a command-line application when executed with `java -jar`.
This way, we can produce an executable that provides the same interface as the lore docker image.