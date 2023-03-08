# LoRe Artifact

The goal of this artifact is to evaluate the performance of the verification process used by the LoRe compiler.

## Importing the Docker Images

```sh
$ docker load -i viper.docker.tar
$ docker load -i lore.docker.tar
```

## Compilation to Viper IR

```sh
$ docker run --rm --volume "$PWD":/pwd --workdir /pwd localhost/lore viperize calendar_new.lore -o calendar_new.vpr
```

## Verification with Viper

```sh
$ docker run --rm --volume "$PWD":/pwd --workdir /pwd localhost/viper calendar_new.vpr
```