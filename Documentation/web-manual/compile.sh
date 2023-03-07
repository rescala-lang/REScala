#!/usr/bin/env bash

cs launch org.scalameta:mdoc_2.13:2.3.7 -- --classpath $(cs fetch --classpath de.tu-darmstadt.stg:rescala_2.13:0.33.0) --in manual-src.md --out manual.md
