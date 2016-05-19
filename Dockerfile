FROM ubuntu:16.04
RUN apt-get update
RUN apt-get install -y openjdk-8-jdk-headless perl git curl
ADD . /rescala
RUN cd /rescala/Microbench && perl manage.pl init chatServer
CMD cd /rescala/Microbench && perl manage.pl run chatServer
