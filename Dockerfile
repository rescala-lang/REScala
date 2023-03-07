FROM alpine:3.17.2
ENV TIME="Time: %E"
COPY target/native-image/lore . 
ENTRYPOINT ["time", "/lore"]