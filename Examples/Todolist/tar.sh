#! /bin/sh

tar -cvf todomvc-rescala.tar \
  node_modules/todomvc-app-css/index.css \
  node_modules/todomvc-common/base.css \
  node_modules/todomvc-common/base.js \
  target/scala-2.12/todolist-fastopt.js \
  target/scala-2.12/todolist-fastopt.js.map \
  index.html

