#!/usr/bin/env bash

cs launch org.scalameta:mdoc_2.12:2.2.20 -- --classpath $(cs fetch --repository bintray:stg-tud/maven --classpath de.tuda.stg:rescala_2.12:0.24.0) --in manual-src.md --out manual.md
