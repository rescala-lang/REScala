ARG ARCH=

FROM ${ARCH}rust:1.76.0-bullseye as builder
WORKDIR /root
RUN rustup component add rustfmt
RUN git clone https://github.com/dtn7/dtn7-rs  && cd dtn7-rs && \
    git checkout 3e06609 && \
    cargo install --locked --bins --examples --root /usr/local --path examples && \
    cargo install --locked --bins --examples --root /usr/local --path core/dtn7
RUN cargo install --locked --bins --examples --root /usr/local dtn7-plus --git https://github.com/dtn7/dtn7-plus-rs  --rev 010202e56 dtn7-plus

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    apt-get install -y libpango1.0-dev libatk1.0-dev libsoup2.4-dev libwebkit2gtk-4.0-dev cmake && \
    rm -rf /var/lib/apt/lists/*
RUN cargo install --bins --examples --root /usr/local --git https://github.com/gh0st42/coreemu-rs --rev 326a6f7

RUN git clone https://github.com/stg-tud/dtn-dwd && cd dtn-dwd && \
    git checkout b78e241 && \
    cargo install --root /usr/local --path backend/ && \
    cargo install --root /usr/local --path client/

RUN git clone https://github.com/gh0st42/dtnchat && cd dtnchat && \
    git checkout 93f1450 && \
    cargo install --bins --examples --root /usr/local --path .


FROM ${ARCH}golang:1.19 as gobuilder

ARG wtfversion=v0.41.0

RUN git clone https://github.com/wtfutil/wtf.git $GOPATH/src/github.com/wtfutil/wtf && \
    cd $GOPATH/src/github.com/wtfutil/wtf && \
    git checkout $wtfversion

ENV GOPROXY=https://proxy.golang.org,direct
ENV GO111MODULE=on
ENV GOSUMDB=off

WORKDIR $GOPATH/src/github.com/wtfutil/wtf

ENV PATH=$PATH:./bin

RUN make build && \
    cp bin/wtfutil /usr/local/bin/


FROM ${ARCH}gh0st42/coreemu-lab:1.1.0

# install stuff for vnc session

ENV DEBIAN_FRONTEND noninteractive
RUN apt clean && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get update && \
    apt-get install -y --fix-missing lxde-core lxterminal \
    tightvncserver firefox wmctrl xterm websockify \
    gstreamer1.0-plugins-bad gstreamer1.0-libav gstreamer1.0-gl \
    pan tin slrn thunderbird \
    && rm -rf /var/lib/apt/lists/*

RUN curl -sL https://deb.nodesource.com/setup_21.x | bash && \
    apt-get install -y nodejs && \
    npm i -g node-red && \
    npm i -g node-red-dashboard && \
    npm i -g node-red-node-ui-list && \
    npm i -g node-red-contrib-web-worldmap && \
    npm i -g node-red-contrib-proj4 && \
    mkdir -p /root/nodered/ && \
    cd /root/nodered/ && \
    #    wget https://raw.githubusercontent.com/dtn7/dtn7-plus-rs/master/extra/dtnmap.json && \
    rm -rf /var/lib/apt/lists/*

COPY configs/dtnmap.json /root/nodered/
COPY --from=builder /usr/local/bin/* /usr/local/bin/

COPY --from=gobuilder /usr/local/bin/* /usr/local/bin/

RUN touch /root/.Xresources
RUN touch /root/.Xauthority
WORKDIR /root
RUN mkdir .vnc Desktop
COPY configs/Xdefaults /root/.Xdefaults
COPY scripts/fakegps.sh /usr/local/bin/fakegps.sh
COPY scripts/dtn7-* /usr/local/bin/

RUN mkdir -p /root/.core/myservices && mkdir -p /root/.coregui/custom_services && mkdir -p /root/.coregui/icons
COPY core_services/* /root/.core/myservices/
COPY coregui/config.yaml /root/.coregui/
COPY coregui/icons/* /root/.coregui/icons/
COPY scenarios/*.xml /root/.coregui/xmls/
COPY configs/dtn7.yml /root/


# --------------------------- moNNT.py Installation ---------------------------

COPY configs/dtnnntp /root/

RUN pip install poetry==1.1.13 && \
    mkdir -p /app/moNNT.py && \
    git clone https://github.com/teschmitt/moNNT.py.git /app/moNNT.py && \
    cd /app/moNNT.py && git checkout 4ed3d9b && \
    mv /root/monntpy-config.py /app/moNNT.py/backend/dtn7sqlite/config.py && \
    mv /root/db.sqlite3 /app/moNNT.py
WORKDIR /app/moNNT.py

RUN poetry install --no-interaction --no-ansi --no-root --no-dev

COPY scripts/dtnnntp-refresher.py /app
ENV DB_PATH="/app/moNNT.py/db.sqlite3" \
    NNTPPORT=1190 \
    NNTPSERVER=10.0.0.21


# --------------------------- dtn7zero Installation ---------------------------
RUN sudo pip3 install --upgrade requests && pip3 install dtn7zero==0.0.8 && sudo apt install python3-matplotlib

# pre-load and parse demo temperature data, will be stored in /app/data/temps.json
RUN mkdir -p /root/.coregui/scripts
COPY scripts/prepare_weather_data.py /root/.coregui/scripts
RUN python3 /root/.coregui/scripts/prepare_weather_data.py


# -----------------------------------------------------------------------------

# add new user for tunneling into gateway node in DTN-NNTP scenario
# RUN useradd --create-home --no-log-init --shell /bin/bash joe
# USER joe
# RUN ssh-keygen -t rsa -f "$HOME/.ssh/id_rsa" -N ""
# USER root

# COPY xstartup with start for lxde
COPY configs/xstartup /root/.vnc/
COPY scripts/coreemu.sh /root/Desktop
COPY scripts/entrypoint.sh /entrypoint.sh

WORKDIR /root
RUN git clone https://github.com/noVNC/noVNC.git 
RUN echo "export USER=root" >> /root/.bashrc
ENV USER root
RUN printf "sneakers\nsneakers\nn\n" | vncpasswd

EXPOSE 22
EXPOSE 1190
EXPOSE 5901
EXPOSE 50051
EXPOSE 6080
ENTRYPOINT [ "/entrypoint.sh" ]
WORKDIR /root


# ------------------------------------------------------------------------------

# thesis configuration
WORKDIR /root

# add openjdk 11
RUN sudo apt-get update
RUN sudo apt-get -y install openjdk-11-jdk

# workdir setup
RUN mkdir /root/temp/
WORKDIR /root/temp

# sbt setup
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
RUN sudo apt-get update
RUN sudo apt-get install sbt
# first time sbt execution will load one-time sbt dependencies
RUN sbt exit

# graalvm setup
RUN wget https://download.oracle.com/graalvm/22/latest/graalvm-jdk-22_linux-x64_bin.tar.gz
RUN tar -xvzf graalvm-jdk-22_linux-x64_bin.tar.gz
RUN rm graalvm-jdk-22_linux-x64_bin.tar.gz

# project clone
RUN git clone https://github.com/lh70/lecture-chat-example.git

# compile native
WORKDIR /root/temp/lecture-chat-example
RUN sbt stageJars
RUN /root/temp/graalvm-jdk-22.0.1+8.1/bin/native-image --class-path /root/temp/lecture-chat-example/jvm/target/jars/"*" dtn.rdt_tool -o /root/.coregui/scripts/rdt_tool --no-fallback --gc=G1 -Ob


WORKDIR /root

