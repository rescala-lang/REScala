#!/bin/bash

// starts two dtnd clients. to be used with "start_direct_routing()" and "send_direct_package()" to test the DirectRouter.
// will create a bundle on node3000 which should be direct-routed to node4000


/home/kali/dtn7-rs/target/release/dtnd -d -U -E 192.168.217.255:3003 -r external -n node3000 -w 3000 -C mtcp:port=3001 &
/home/kali/dtn7-rs/target/release/dtnd -d -U -E 192.168.217.255:3003 -r epidemic -n node4000 -w 4000 -C mtcp:port=4001 &
wait
