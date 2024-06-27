#!/bin/bash

systemctl enable ssh
service ssh start

/update-custom-services.sh
#service core-daemon start
core-daemon > /var/log/core-daemon.log 2>&1 &

iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE

if [ ! -z "$SSHKEY" ]; then
    echo "Adding ssh key: $SSHKEY"
    mkdir /root/.ssh
    chmod 755 ~/.ssh
    echo $SSHKEY > /root/.ssh/authorized_keys
    chmod 644 /root/.ssh/authorized_keys
fi

if test -f "/tmp/.X1-lock"; then
    rm /tmp/.X1-lock
    rm /tmp/.X11-unix/X1
fi

# /usr/bin/tightvncserver -geometry 1280x800 -depth 24 &
/usr/bin/tightvncserver -geometry 1920x1080 -depth 24 &
# /usr/bin/tightvncserver -geometry 2560x1080 -depth 24 &

/usr/local/bin/fakegps.sh > /tmp/fakegps.log 2>&1 &

/root/noVNC/utils/novnc_proxy --vnc localhost:5901 2>&1 > /tmp/novnc.log &

echo "vnc://127.0.0.1:5901"
echo "http://127.0.0.1:6080/vnc.html"
tail -f /dev/null