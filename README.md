[![Chat](https://img.shields.io/matrix/dtn7:matrix.org)](https://matrix.to/#/#dtn7:matrix.org)

# dtn7 showroom

This Docker image contains a virtual showroom and playground for various applications using [dtn7-rs](https://github.com/dtn7/dtn7-rs).
To play with the container docker on arm64 or x64 is needed as well as a VNC client.

Currently, the following example scenarios are included:

- [dtn dwd](https//github.com/stg-tud/dtn-dwd): delay-tolerant weather warnings
- [dtnchat](https://github.com/gh0st42/dtnchat): a simple text-based dtn chat including a chatbot
- [NNTP DTN](https://github.com/teschmitt/moNNT.py): a NNTP-to-DTN gateway for group discussions using thunderbard, pan, etc.
- [dtn7zero](https://github.com/dtn7/dtn7zero): IoT sensor reading and value plotting over DTN, using dtn7zero and dtn7-rs nodes

## Running 

Only the `dtn7showroom.sh` script or docker itself are needed:
```
$ docker run --rm -it --name showroom -p 5901:5901 --privileged -v /tmp/shared:/shared gh0st42/dtn7-showroom

'########::'########:'##::: ##:'########::'######::'##::::'##::'#######::'##:::::'##:'########:::'#######:::'#######::'##::::'##:
 ##.... ##:... ##..:: ###:: ##: ##..  ##:'##... ##: ##:::: ##:'##.... ##: ##:'##: ##: ##.... ##:'##.... ##:'##.... ##: ###::'###:
 ##:::: ##:::: ##:::: ####: ##:..:: ##::: ##:::..:: ##:::: ##: ##:::: ##: ##: ##: ##: ##:::: ##: ##:::: ##: ##:::: ##: ####'####:
 ##:::: ##:::: ##:::: ## ## ##:::: ##::::. ######:: #########: ##:::: ##: ##: ##: ##: ########:: ##:::: ##: ##:::: ##: ## ### ##:
 ##:::: ##:::: ##:::: ##. ####::: ##::::::..... ##: ##.... ##: ##:::: ##: ##: ##: ##: ##.. ##::: ##:::: ##: ##:::: ##: ##. #: ##:
 ##:::: ##:::: ##:::: ##:. ###::: ##:::::'##::: ##: ##:::: ##: ##:::: ##: ##: ##: ##: ##::. ##:: ##:::: ##: ##:::: ##: ##:.:: ##:
 ########::::: ##:::: ##::. ##::: ##:::::. ######:: ##:::: ##:. #######::. ###. ###:: ##:::. ##:. #######::. #######:: ##:::: ##:
........::::::..:::::..::::..::::..:::::::......:::..:::::..:::.......::::...::...:::..:::::..:::.......::::.......:::..:::::..::

==> vnc://127.0.0.1:5901
==> password: sneakers

[...]
```

You can then connect with any VNC client to the local *dtn7 showroom* instance with the password `sneakers`.

*NOTE:* In case of weird connection problems within the showroom, please make sure that *ebtables* and *sch_netem* kernel modules are loaded!

## Manually building the container

Just run `docker build -t dtn7-showroom .` and run it with `docker run --rm -it --name showroom -p 5901:5901 --privileged -v /tmp/shared:/shared dtn7-showroom`

