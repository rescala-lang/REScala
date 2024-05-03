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


// /home/kali/dtn7-rs/target/release/dtnd -d -U -E 192.168.217.255:3003 -r external -n node3000 -w 3000 -C mtcp:port=3001
// /home/kali/dtn7-rs/target/release/dtnd -d -U -E 192.168.217.255:3003 -r epidemic -n node4000 -w 4000 -C mtcp:port=4001


// setup for a minimal 1-to-1 rdt package exchange
//
// start the following:
// /home/kali/dtn7-rs/target/release/dtnd -d -U -E 192.168.217.255:3003 -r external -n node3000 -w 3000 -C mtcp:port=3001
// /home/kali/dtn7-rs/target/release/dtnd -d -U -E 192.168.217.255:3003 -r external -n node4000 -w 4000 -C mtcp:port=4001
// 
// start the routing:
// start_rdtdots_routing_3000
// start_rdtdots_routing_4000
//
// send one package after another
// send_one_rdt_package_3000
// send_one_rdt_package_4000

