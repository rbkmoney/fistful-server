#!/bin/bash
cat <<EOF
version: '2.1'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: |
      bash -c '{
        woorl -s _build/default/lib/dmsl/proto/cds.thrift http://cds:8022/v1/keyring Keyring Init 1 1 || true;
        exec /sbin/init
      }'
    depends_on:
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

  hellgate:
    image: dr.rbkmoney.com/rbkmoney/hellgate:152732ac4a3121c836b352354a29bcb7a87ab61c
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
      - ./test/log/hellgate:/var/log/hellgate
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  adapter-vtb:
    depends_on:
      - cds
    image: dr.rbkmoney.com/rbkmoney/proxy-vtb-mpi-vtb:80eb990920d34ee74c41c39168357dab9e42bd70
    command: |
      java
      -Xms64m -Xmx256m
      -jar /opt/proxy-vtb-mpi-vtb/proxy-vtb-mpi-vtb.jar
      --logging.file=/var/log/proxy-vtb-mpi-vtb/proxy-vtb-mpi-vtb.json
      --server.secondary.ports=8080
      --server.port=8022
      --cds.url.storage=http://cds:8022/v1/storage
      --cds.url.idStorage=http://cds:8022/v1/identity_document_storage
      --hellgate.url=http://hellgate:8022/v1/proxyhost/provider
      --vtb.paymentUrl=null
      --vtb.p2pUrl=https://mishop02.multicarta.ru:7070/extproc/posdh_p2p_visapit.php
      --vtb.callbackUrl=http://proxy-vtb-mpi-vtb:8080
      --vtb.pathCallbackUrl=/vtb-mpi-vtb/term_url{?termination_uri}
    environment:
      - KEYSTORE_PAYMENT_CERTIFICATE=file:/opt/proxy-vtb-mpi-vtb/cert/cert.p12
      - KEYSTORE_PAYMENT_PASSWORD=12345
      - KEYSTORE_PAYMENT_TYPE=pkcs12
      - KEYSTORE_P2P_CERTIFICATE=file:/opt/proxy-vtb-mpi-vtb/cert/p2p.p12
      - KEYSTORE_P2P_PASSWORD=12345
      - KEYSTORE_P2P_TYPE=pkcs12
    volumes:
      - ./test/adapter-vtb/cert.p12:/opt/proxy-vtb-mpi-vtb/cert/cert.p12
      - ./test/adapter-vtb/p2p.p12:/opt/proxy-vtb-mpi-vtb/cert/p2p.p12
      - ./test/log/adapter-vtb:/var/log/proxy-vtb-mpi-vtb
    working_dir: /opt/proxy-vtb-mpi-vtb
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:2c4c8aef2de8b55dfe6ea43e919b967ca649d98d
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy
    volumes:
      - ./test/dominant/sys.config:/opt/dominant/releases/0.1/sys.config
      - ./test/log/dominant:/var/log/dominant
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:862509b10a637d9b7ea739abd56bc6b18cf25296
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
    depends_on:
      - shumway-db
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  identification:
    image: dr.rbkmoney.com/rbkmoney/identification:0578a5f76a88c7c2e9b5a3043641c028779c8921
    command: /opt/identification/bin/identification foreground
    volumes:
      - ./test/identification/sys.config:/opt/identification/releases/0.1/sys.config
      - ./test/log/identification:/var/log/identification
    depends_on:
      - cds
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:dcefbd80df9ac9b240ce3033b55e87b0cd85abba
    command: /opt/cds/bin/cds foreground
    volumes:
      - ./test/cds/sys.config:/opt/cds/releases/0.1.0/sys.config
      - ./test/log/cds:/var/log/cds
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:27df9e276102d5c9faf8d1121374c7355d8e2d1b
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
      - ./test/log/machinegun:/var/log/machinegun
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  shumway-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumway-db

EOF
