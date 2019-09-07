# base image

arg from
from ${from}

# arguments

arg name
arg UID
arg GID

# user setup

env USER=docker
env HOME=/usr/src/${name}/

run addgroup --gid "$GID" "$USER"

run adduser --disabled-password \
            --gecos "" \
            --home "$HOME" \
            --no-create-home \
            --ingroup "$USER" \
            --uid "$UID" \
            "$USER"

run install -d -o docker -g docker "$HOME"

user "$USER"

workdir ${HOME}

# project setup

volume /usr/src/${name}/

cmd make
