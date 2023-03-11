FROM docker.io/sbtscala/scala-sbt:eclipse-temurin-jammy-19.0.1_10_1.8.2_3.2.2
COPY target/native-image/lore /lore
COPY src /app/src 
COPY project /app/project
COPY build.sbt /app/build.sbt

# requirements for fish
RUN ["apt-get", "update"]
RUN ["apt-get", "install", "-y", "wget", "python3", "libpcre2-32-0", "gettext-base", "man-db"]
RUN ["wget", "https://launchpad.net/~fish-shell/+archive/ubuntu/release-3/+files/fish_3.6.0-1~jammy_amd64.deb"]
RUN ["dpkg", "-i" ,"fish_3.6.0-1~jammy_amd64.deb"]
COPY compile_benchmarks.fish /compile_benchmarks

# needed to include all the dependecies in the image
WORKDIR /app
RUN ["sbt", "compile"]
RUN ["sbt", "assembly"]

WORKDIR /pwd
ENTRYPOINT ["/lore"]