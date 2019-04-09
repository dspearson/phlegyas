FROM oracle/graalvm-ce:1.0.0-rc14 AS BASE

ADD . /target
WORKDIR /target
RUN curl -o /lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
RUN chmod +x /lein
RUN /lein native-image
