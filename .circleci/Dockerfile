ARG resolver

FROM fpco/stack-build:$resolver

ARG stack_version

RUN apt-get update && apt-get install -y \
    build-essential \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://get.haskellstack.org/ | sh -s - -f
RUN stack upgrade --binary-version $stack_version

WORKDIR /opt/server

COPY code/package.yaml /opt/server/package.yaml
COPY code/stack.yaml /opt/server/stack.yaml

RUN stack setup
RUN stack test --only-dependencies