FROM scratch
ENV TIME="Time: %E"
COPY target/native-image/lore . 
#ENTRYPOINT ["time", "/lore"]
ENTRYPOINT ["/lore"]