FROM debian:latest

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils lsof strace vim procps tree bc openjdk-8-jre-headless openjdk-8-jdk-headless

ENV PATH=/opt/scala-2.12.6/bin:${PATH} \
    LANG=C.UTF-8 \
    JAVA_HOME=/usr/bin \ 
    JAVA_VERSION=8u171 \
    JAVA_DEBIAN_VERSION=8u171-b11-1~deb9u1-b11 

RUN useradd -u 1000 -m -d /home/sbt -s /bin/bash sbt
COPY sbt-1.1.6.deb /tmp/ 
RUN dpkg -i /tmp/sbt-1.1.6.deb

COPY scala-2.12.6.tgz /tmp/scala-2.12.6.tgz
RUN tar -C /opt -zxvf /tmp/scala-2.12.6.tgz

COPY home/sbt /home/sbt
RUN chown -R sbt:sbt /home/sbt 

WORKDIR /home/sbt
USER sbt
