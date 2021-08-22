#!/bin/bash
cat <<EOF
version: '2.1'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - ./test/${SERVICE_NAME}/private.pem:/opt/${SERVICE_NAME}/config/private.pem
      - ./test/${SERVICE_NAME}/jwk.publ.json:/opt/${SERVICE_NAME}/config/jwk.publ.json
      - ./test/${SERVICE_NAME}/jwk.priv.json:/opt/${SERVICE_NAME}/config/jwk.priv.json
      - ./test/${SERVICE_NAME}/enc.1.priv.json:/opt/${SERVICE_NAME}/config/enc.1.priv.json
      - ./test/${SERVICE_NAME}/sig.1.priv.json:/opt/${SERVICE_NAME}/config/sig.1.priv.json
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      party-management:
        condition: service_healthy
      hellgate:
        condition: service_healthy
      identification:
        condition: service_healthy
      cds:
        condition: service_healthy
      dominant:
        condition: service_healthy
      machinegun:
        condition: service_healthy
      adapter-mocketbank:
        condition: service_healthy

  hellgate:
    image: dr2.rbkmoney.com/rbkmoney/hellgate:ce90c673ba8e334f0615005fe58684223bdd3744
    command: /opt/hellgate/bin/hellgate foreground
    depends_on:
      machinegun:
        condition: service_healthy
      dominant:
        condition: service_healthy
      shumway:
        condition: service_healthy
    volumes:
      - ./test/hellgate/sys.config:/opt/hellgate/releases/0.1/sys.config
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  adapter-mocketbank:
    depends_on:
      - cds
    image: dr2.rbkmoney.com/rbkmoney/proxy-mocketbank:39131ebc714c9a97c692e8c6b656a5938b6ba545
    command: |
      java
        -Xms64m -Xmx256m
        -jar /opt/proxy-mocketbank/proxy-mocketbank.jar
        --logging.file=/var/log/proxy-mocketbank/proxy-mocketbank.json
        --server.rest.port=8080
        --server.port=8022
        --cds.client.storage.url=http://cds:8022/v2/storage
        --cds.client.identity-document-storage.url=http://cds:8022/v1/identity_document_storage
        --hellgate.client.adapter.url=http://hellgate:8022/v1/proxyhost/provider
        --fistful.client.adapter.url=http://fisful-server:8022/v1/ff_p2p_adapter_host

    working_dir: /opt/proxy-mocketbank
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  dominant:
    image: dr2.rbkmoney.com/rbkmoney/dominant:22dad3f2b8655bae2125db116ddd61652160d128
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy
    volumes:
      - ./test/dominant/sys.config:/opt/dominant/releases/0.1/sys.config
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  shumway:
    image: dr2.rbkmoney.com/rbkmoney/shumway:e946e83703e02f4359cd536b15fb94457f9bfb20
    restart: unless-stopped
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
      - --management.metrics.export.statsd.enabled=false
    depends_on:
      - shumway-db
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 30

  identification:
    image: dr2.rbkmoney.com/rbkmoney/identification:1d23c0fa422d2bd0542a08f70cca292f1d6c91eb
    command: /opt/identification/bin/identification foreground
    volumes:
      - ./test/identification/sys.config:/opt/identification/releases/0.1/sys.config
    depends_on:
      - cds
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  cds:
    image: dr2.rbkmoney.com/rbkmoney/cds:c0661c4d5abb85f7728bd0e816760670aa248251
    command: /opt/cds/bin/cds foreground
    volumes:
      - ./test/cds/sys.config:/opt/cds/releases/0.1.0/sys.config
      - ./test/cds/ca.crt:/var/lib/cds/ca.crt:ro
      - ./test/cds/client.pem:/var/lib/cds/client.pem
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10
    depends_on:
      kds:
        condition: service_healthy

  kds:
    image: dr2.rbkmoney.com/rbkmoney/kds:2f62c8b8e6e32a7d76d7f1ef251bcda419eb9e1f
    command: /opt/kds/bin/kds foreground
    volumes:
      - ./test/kds/sys.config:/opt/kds/releases/0.1.0/sys.config:ro
      - ./test/kds/ca.crt:/var/lib/kds/ca.crt:ro
      - ./test/kds/server.pem:/var/lib/kds/server.pem:ro
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  holmes:
    image: dr2.rbkmoney.com/rbkmoney/holmes:07f58e297c03bcd50dc4695ddbcfa4eb30c9928e
    command: /opt/holmes/scripts/cds/keyring.py init
    depends_on:
      - cds

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:0da2ffc23221e1e3f8557b03d48d11d2dd18fac0
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
      - ./test/machinegun/cookie:/opt/machinegun/etc/cookie
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  shumway-db:
    image: dr2.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumway-db

  binbase:
    image: dr2.rbkmoney.com/rbkmoney/binbase-data:fe5b954414e5ca7b07f1cbc1d24b231b307f2cfb
    restart: always
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  party-management:
    image: dr2.rbkmoney.com/rbkmoney/party-management:f161a8103bb85d003b4014ab7bd94744e7f506fa
    command: /opt/party-management/bin/party-management foreground
    depends_on:
      - machinegun
      - dominant
      - shumway
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

EOF
