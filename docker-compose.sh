#!/bin/bash
cat <<EOF
version: '2.1'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - ./apps/wapi/var/keys/wapi/private.pem:/opt/wapi/config/private.pem
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      wapi-pcidss:
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

  wapi-pcidss:
    image: dr.rbkmoney.com/rbkmoney/wapi:cbd351653a16ceb57a67c44cd99d0fbc34cc9c29
    command: /opt/wapi/bin/wapi foreground
    volumes:
      - ./apps/wapi/var/keys/wapi/private.pem:/opt/wapi/var/keys/wapi/private.pem
    depends_on:
      cds:
        condition: service_healthy
    healthcheck:
      test: "curl http://localhost:8080/"
      interval: 5s
      timeout: 1s
      retries: 10

  hellgate:
    image: dr2.rbkmoney.com/rbkmoney/hellgate:4b9804fade0fef5fa0ad8cfe4a9748dc8c5574e7
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

  adapter-mocketbank:
    depends_on:
      - cds
    image: dr.rbkmoney.com/rbkmoney/proxy-mocketbank:fe9b71f013e371e64844078d35179944e82ec1ed
    command: |
      java
      -Xms64m -Xmx256m
      -jar /opt/proxy-mocketbank/proxy-mocketbank.jar
      --logging.file=/var/log/proxy-mocketbank/proxy-mocketbank.json
      --server.secondary.ports=8080
      --server.port=8022
      --cds.url.storage=http://cds:8022/v1/storage
      --cds.url.idStorage=http://cds:8022/v1/identity_document_storage
      --hellgate.url=http://hellgate:8022/v1/proxyhost/provider
    working_dir: /opt/proxy-mocketbank
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:5a2be39e1035bf590af2e2a638062d6964708e05
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
      retries: 30

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
    image: dr2.rbkmoney.com/rbkmoney/cds:f7ad5a34a2f6d0780f44821290ba7c52d349f3f7
    command: /opt/cds/bin/cds foreground
    volumes:
      - ./test/cds/sys.config:/opt/cds/releases/0.1.0/sys.config
      - ./test/log/cds:/var/log/cds
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  holmes:
    image: dr2.rbkmoney.com/rbkmoney/holmes:7a430d6ec97518a0ffe6e6c24ce267390de18b40
    command: /opt/holmes/scripts/cds/keyring.py init
    depends_on:
      - cds

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:bffbaefa679c823d375cbf2b2434f72d2e3e5242
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

  fistful-magista:
    image: dr.rbkmoney.com/rbkmoney/fistful-magista:fed290bccd48627822fda47f9dc2fe0cd1d3a5ad
    restart: always
    entrypoint:
      - java
      - -Xmx256m
      - -jar
      - /opt/fistful-magista/fistful-magista.jar
      - --spring.datasource.url=jdbc:postgresql://ffmagista-db:5432/ffmagista
      - --spring.datasource.username=postgres
      - --withdrawal.polling.url=http://fistful-server:8022/v1/eventsink/withdrawal
      - --identity.polling.url=http://fistful-server:8022/v1/eventsink/identity
      - --wallet.polling.url=http://fistful-server:8022/v1/eventsink/wallet
    depends_on:
      - ffmagista-db
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10
    environment:
      - SPRING_DATASOURCE_PASSWORD=postgres
      - SERVICE_NAME=ffmagista

  ffmagista-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=ffmagista
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=ffmagista-db

EOF
