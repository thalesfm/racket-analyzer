FROM docker.io/debian:bullseye-slim

ARG USERNAME=racket
ARG USER_UID=1000
ARG USER_GID=$USER_UID

COPY xvfb-racket /usr/local/bin/

RUN chmod +x /usr/local/bin/xvfb-racket &&\
    groupadd -g $USER_GID $USERNAME &&\
    useradd -u $USER_UID -g $USER_GID -m $USERNAME &&\
    apt update &&\
    apt install -y ca-certificates racket xvfb


USER racket

CMD ["racket"]
