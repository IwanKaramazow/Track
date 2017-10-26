FROM ubuntu:17.10
MAINTAINER Iwan Karamazow

RUN apt-get update
RUN apt-get install -y git wget

ENV NODE_VERSION 8.9.0

RUN wget https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.xz \
  && tar -xJf "node-v$NODE_VERSION-linux-x64.tar.xz" -C /usr/local --strip-components=1 \
  && rm "node-v$NODE_VERSION-linux-x64.tar.xz"

RUN mkdir -p /out

COPY . /out

WORKDIR /out

RUN npm install -g esy

CMD ["/bin/bash"]
