FROM docker.io/debian:bullseye-slim

ARG USERNAME=racket
ARG USER_UID=1000
ARG USER_GID=$USER_UID

COPY xvfb-racket /usr/local/bin/

RUN groupadd -g $USER_GID $USERNAME &&\
    useradd -u $USER_UID -g $USER_GID -m $USERNAME &&\
    apt update &&\
    apt install -y ca-certificates git racket xvfb &&\
    chmod +x /usr/local/bin/xvfb-racket &&\
    raco pkg install -iD --auto racket-langserver

USER racket

CMD ["racket"]
