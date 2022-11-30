FROM ubuntu:latest

RUN groupadd -g 1000 sbt
RUN useradd -u 1000 -g 1000 -m -d /home/sbt sbt

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends less strace iproute2 vim lsof bash-completion git apt-transport-https curl gnupg ca-certificates  

RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" >/etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | >/etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scala-sbt-release.gpg --import
RUN chmod 644 /etc/apt/trusted.gpg.d/scala-sbt-release.gpg

RUN apt-get update && apt-get install -y --no-install-recommends sbt openjdk-11-jdk fonts-freefont-ttf

ENV LANG=C.UTF-8
WORKDIR /home/sbt
USER sbt
