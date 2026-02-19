FROM sbtscala/scala-sbt:eclipse-temurin-25.0.1_8_1.12.3_3.8.1
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends less strace iproute2 vim lsof bash-completion git apt-transport-https curl gnupg ca-certificates  

WORKDIR /home/sbtuser
USER sbtuser
ENTRYPOINT ["/bin/bash"]
