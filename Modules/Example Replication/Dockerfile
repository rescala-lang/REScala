# syntax=docker/dockerfile:1
FROM ubuntu:22.04
LABEL Description="CORE Docker Ubuntu Image"

# define variables
ARG PREFIX=/usr/local
ARG BRANCH=release-9.0.1

# define environment
ENV DEBIAN_FRONTEND=noninteractive

# install basic dependencies
RUN apt-get update && \
	apt-get install -y git sudo wget tzdata

RUN apt-get install -y curl

# install scala
RUN curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs && chmod +x cs
RUN ./cs setup --jvm 17 --yes

# install core
WORKDIR /opt
RUN git clone https://github.com/coreemu/core
WORKDIR /opt/core
RUN git checkout ${BRANCH}
RUN ./setup.sh
RUN . /root/.bashrc && inv install -v -p ${PREFIX}
ENV PATH "$PATH:/opt/core/venv/bin"

# install emane
# RUN apt-get install -y libpcap-dev libpcre3-dev libprotobuf-dev libxml2-dev protobuf-compiler unzip uuid-dev
# WORKDIR /opt
# RUN git clone https://github.com/adjacentlink/emane.git
# RUN cd emane && \
#   ./autogen.sh && \
#   ./configure --prefix=/usr && \
#   make -j$(nproc)  && \
#   make install
# RUN wget https://github.com/protocolbuffers/protobuf/releases/download/v3.19.6/protoc-3.19.6-linux-x86_64.zip && \
#     mkdir protoc && \
#     unzip protoc-3.19.6-linux-x86_64.zip -d protoc
# RUN PATH=/opt/protoc/bin:$PATH && \
#   cd emane/src/python && \
#   make clean && \
#   make
# RUN /opt/core/venv/bin/python -m pip install emane/src/python

# run daemon
CMD ["core-daemon"]
