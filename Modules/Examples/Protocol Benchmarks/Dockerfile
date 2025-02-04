FROM openjdk:21-jdk-slim

RUN apt-get update -y
RUN apt-get install unzip

WORKDIR /project

RUN ls

COPY target/universal/probench.zip .
RUN unzip probench.zip
RUN rm probench.zip

ENTRYPOINT ["/project/probench/bin/probench"]
