#!/usr/bin/env bash

cs launch org.scalameta:mdoc_3:2.5.3 -- --classpath $(cs fetch --classpath de.tu-darmstadt.stg:rescala_3:0.35.1) --in manual-src.md --out manual.md
