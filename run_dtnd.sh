#!/bin/bash

// starts multiple dtnd with neighbour discovery to be used as nodes for the chat-clients
// each chat client should connect to one node

start_dtnd () {
  /home/kali/dtn7-rs/target/release/dtnd -d -U -E 192.168.217.255:3003 -r epidemic -n node$1 -w $1 -C mtcp:port=$(($1 + 1))
}

start_dtnd 3000 &
start_dtnd 4000 &
start_dtnd 5000 & 
wait
