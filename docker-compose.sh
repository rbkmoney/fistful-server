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
    image: dr.rbkmoney.com/rbkmoney/hellgate:eae821f2a1f3f2b948390d922c5eb3cde885757d
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
    image: dr.rbkmoney.com/rbkmoney/proxy-vtb-mpi-vtb:e28626d5f6fa07c08c71bde1e8089acad2b18d6d
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
    image: dr.rbkmoney.com/rbkmoney/dominant:4e296b03cd4adba4bd0f1cf85425b9514728107c
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
    image: dr.rbkmoney.com/rbkmoney/shumway:7a5f95ee1e8baa42fdee9c08cc0ae96cd7187d55
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
    image: dr.rbkmoney.com/rbkmoney/identification:ff4ef447327d81882c0ee618b622e5e04e771881
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
    image: dr.rbkmoney.com/rbkmoney/cds:a02376ae8a30163a6177d41edec9d8ce2ff85e4f
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
    image: dr.rbkmoney.com/rbkmoney/machinegun:5756aa3070f9beebd4b20d7076c8cdc079286090
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
