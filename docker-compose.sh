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

    working_dir: /opt/proxy-mocketbank
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  dominant:
    image: dr2.rbkmoney.com/rbkmoney/dominant:753f3e0711fc7fff91abcad6e279225a7e5b8b8c
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
    image: dr2.rbkmoney.com/rbkmoney/shumway:44eb989065b27be619acd16b12ebdb2288b46c36
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
    image: dr2.rbkmoney.com/rbkmoney/cds:6e6541c99d34b0633775f0c5304f5008e6b2aaf3
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
    image: dr2.rbkmoney.com/rbkmoney/kds:e37c7bbc0e9dd485a9c5a094c3c6e631ef3af110
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
    image: dr2.rbkmoney.com/rbkmoney/machinegun:9c3248a68fe530d23a8266057a40a1a339a161b8
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
    image: dr2.rbkmoney.com/rbkmoney/party-management:f55197723b34e3be30b1e3dc0d57b948db8e2062
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
