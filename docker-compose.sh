#!/bin/bash
cat <<EOF
version: '2.1'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - ./apps/wapi/var/keys/wapi/private.pem:/opt/wapi/config/private.pem
      - ./apps/wapi/var/keys/wapi/jwk.publ.json:/opt/wapi/config/jwk.publ.json
      - ./apps/wapi/var/keys/wapi/jwk.priv.json:/opt/wapi/config/jwk.priv.json
      - ./apps/wapi/var/keys/wapi/enc.1.priv.json:/opt/wapi/config/enc.1.priv.json
      - ./apps/wapi/var/keys/wapi/sig.1.priv.json:/opt/wapi/config/sig.1.priv.json
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
    image: dr2.rbkmoney.com/rbkmoney/wapi:9ae84a966a29937ed3440fe773ef8bf6c280301c
    command: /opt/wapi/bin/wapi foreground
    volumes:
      - ./test/wapi/sys.config:/opt/wapi/releases/0.0.1/sys.config
      - ./apps/wapi/var/keys/wapi/private.pem:/opt/wapi/var/keys/wapi/private.pem
      - ./apps/wapi/var/keys/wapi/jwk.publ.json:/opt/wapi/var/keys/wapi/jwk.publ.json
      - ./apps/wapi/var/keys/wapi/jwk.priv.json:/opt/wapi/var/keys/wapi/jwk.priv.json
      - ./test/log/wapi:/var/log/wapi
    depends_on:
      cds:
        condition: service_healthy
      binbase:
        condition: service_healthy
    healthcheck:
      test: "curl http://localhost:8080/"
      interval: 5s
      timeout: 1s
      retries: 10

  hellgate:
    image: dr2.rbkmoney.com/rbkmoney/hellgate:983ba4d48b47cd5216f75cb3e30ab14f1dd99f46
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
        --fistful.client.adapter.url=http://wapi:8022/v1/ff_p2p_adapter_host

    working_dir: /opt/proxy-mocketbank
    volumes:
        - ./test/log/proxy-mocketbank:/var/log/proxy-mocketbank
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  dominant:
    image: dr2.rbkmoney.com/rbkmoney/dominant:1313973ee38e30116d14aa007cdf551f702900f5
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
    image: dr2.rbkmoney.com/rbkmoney/shumway:658c9aec229b5a70d745a49cb938bb1a132b5ca2
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
      - ./test/log/identification:/var/log/identification
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
      - ./test/log/cds:/var/log/cds
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
      - ./test/log/kds:/var/log/kds
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20

  holmes:
    image: dr2.rbkmoney.com/rbkmoney/holmes:55e745b7c020c367bff202036af84726d66755f7
    command: /opt/holmes/scripts/cds/keyring.py init
    depends_on:
      - cds

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:c35e8a08500fbc2f0f0fa376a145a7324d18a062
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
      - ./test/log/machinegun:/var/log/machinegun
      - ./test/machinegun/cookie:/opt/machinegun/etc/cookie
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 10

  bender:
    image: dr2.rbkmoney.com/rbkmoney/bender:cd0ee8faae41f22a40ea119337be2a842e3e9cd8
    command: /opt/bender/bin/bender foreground
    volumes:
      - ./test/log/bender:/var/log/bender
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 20
    depends_on:
      - machinegun

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

  fistful-magista:
    image: dr2.rbkmoney.com/rbkmoney/fistful-magista:ae8a1ccdddcf75827251d9011f4f340baaaeafa8
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
    image: dr2.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=ffmagista
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=ffmagista-db

EOF
