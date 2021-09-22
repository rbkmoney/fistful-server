%% -*- mode: erlang -*-
-module(swag_server_wallet_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_server_wallet:object().
get() ->
    ct_expand:term(enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw()))).

-spec enumerate_discriminator_children(Schema :: map()) ->
    Schema :: map() | no_return().
enumerate_discriminator_children(Schema = #{?DEFINITIONS := Defs}) ->
    try
        {Parents, _} = maps:fold(fun check_definition/3, {#{}, #{}}, Defs),
        maps:fold(fun correct_schema/3, Schema, Parents)
    catch
        _:Error ->
            handle_error(Error)
    end;
enumerate_discriminator_children(_) ->
    handle_error(no_definitions).

-spec handle_error(_) ->
    no_return().
handle_error(Error) ->
    erlang:error({schema_invalid, Error}).

check_definition(Name, Schema, Acc) ->
    Acc1 = check_discriminator(Name, Schema, Acc),
    check_backrefs(Name, Schema, Acc1).

check_discriminator(Name, Schema, {Parents, Candidates}) ->
    case maps:get(<<"discriminator">>, Schema, undefined) of
        undefined ->
            {Parents, Candidates};
        _ ->
            {
                Parents#{Name => maps:get(Name, Candidates, [])},
                maps:without([Name], Candidates)
            }
    end.

check_backrefs(Name, Schema, Acc) ->
    case maps:get(<<"allOf">>, Schema, undefined) of
        undefined ->
            Acc;
        AllOf ->
            lists:foldl(fun(E, A) -> check_allOf(E, Name, A) end, Acc, AllOf)
    end.

check_allOf(#{<<"$ref">> := <<"#/definitions/", Parent/binary>>}, Child, {Parents, Candidates}) ->
    case maps:get(Parent, Parents, undefined) of
        undefined ->
            {Parents, update_candidates(Parent, Child, Candidates)};
        Children ->
            {Parents#{Parent => [Child | Children]}, Candidates}
    end;
check_allOf(_, _, Acc) ->
    Acc.

update_candidates(Parent, Child, Candidates) ->
    case maps:get(Parent, Candidates, undefined) of
        undefined ->
            Candidates#{Parent => [Child]};
        Children ->
            Candidates#{Parent => [Child | Children]}
    end.

correct_schema(Parent, Children, Schema) ->
    BasePath = [Parent, ?DEFINITIONS],
    Discr    = maps:get(<<"discriminator">>, get_sub_schema(BasePath, Schema)),
    update_schema(Children, [<<"enum">>, Discr, <<"properties">> | BasePath], Schema).

update_schema(Value, [], _Schema) ->
    Value;
update_schema(Value, [Key | Path], Schema) ->
    SubSchema0 = get_sub_schema(Path, Schema),
    SubSchema1 = update_sub_schema(Key, Value, SubSchema0),
    update_schema(SubSchema1, Path, Schema).

get_sub_schema(ReversedPath, Schema) ->
    lists:foldr(fun(K, S) -> maps:get(K, S) end, Schema, ReversedPath).

update_sub_schema(Key, Value, Schema) ->
    Schema#{Key => Value}.

-spec get_raw() -> map().
get_raw() ->
    #{
  <<"swagger">> => <<"2.0">>,
  <<"info">> => #{
    <<"description">> => <<"\nRBKmoney Wallet API является базовой и единственной точкой взаимодействия с платформой кошельков. Все изменения состояний платформы осуществляются с помощью вызовов соответствующих методов API. Любые сторонние приложения, включая наши веб-сайты и другие UI-интерфейсы являются внешними приложениями-клиентами.\nRBKmoney API работает поверх HTTP-протокола. Мы используем REST архитектуру, схема описывается в соответствии со [Swagger](http://swagger.io/). Коды возврата описываются соответствующими HTTP-статусами. Платформа принимает и возвращает значения JSON в теле запросов и ответов.\n## Формат содержимого\nЛюбой запрос к API должен выполняться в кодировке UTF-8 и с указанием содержимого в формате JSON.\n```\n  Content-Type: application/json; charset=utf-8\n```\n## Формат дат\nПлатформа принимает значения date-time в формате, описанном в RFC 3339, например:\n```\n  2017-01-01T00:00:00Z\n  2017-01-01T00:00:01+00:00\n```\n## Максимальное время обработки запроса\nК любому вызову методов API можно добавить параметр отсечки по времени, определяющий максимальное время ожидания завершения операции по запросу. Данная отсечка передается в соответствующем заголовке HTTP-запроса:\n```\n X-Request-Deadline: 10s\n```\nЗначение отсечки может быть задано в формате, описанном в RFC 3339 (см. [Формат дат](#section/Format-dat)), либо в относительных величинах, например:\n`150000ms`, `540s`, `3.5m` При этом возможные единицы измерения `ms`, `s`, `m`. В обоих случаях  не рекомендуется, чтобы задаваемое значение было меньше **3 секунд** и превышало **1 минуту**.\n## Ошибки обработки запросов\nВ процессе обработки запросов силами нашей платформы могут происходить различные непредвиденные ситуации. Об их появлении платформа сигнализирует по протоколу HTTP соответствующими [статусами][5xx], обозначающими ошибки сервера.\n\n |  Код    |  Описание  |\n | ------- | ---------- |\n | **500** | В процессе обработки платформой запроса возникла непредвиденная ситуация. При получении подобного кода ответа мы рекомендуем обратиться в техническую поддержку. |\n | **503** | Платформа временно недоступна и не готова обслуживать данный запрос. Запрос гарантированно не выполнен, при получении подобного кода ответа попробуйте выполнить его позднее, когда доступность платформы будет восстановлена. |\n | **504** | Платформа превысила допустимое время обработки запроса, результат запроса не определён. Попробуйте отправить запрос повторно или выяснить результат выполнения исходного запроса, если повторное исполнение запроса нежелательно. |\n\n[5xx]: https://tools.ietf.org/html/rfc7231#section-6.6\n">>,
    <<"version">> => <<"0.0.1">>,
    <<"title">> => <<"RBKmoney Wallet API">>,
    <<"termsOfService">> => <<"http://rbk.money/">>,
    <<"contact">> => #{
      <<"name">> => <<"RBKmoney support team">>,
      <<"url">> => <<"https://rbk.money">>,
      <<"email">> => <<"support@rbk.money">>
    }
  },
  <<"host">> => <<"api.rbk.money">>,
  <<"basePath">> => <<"/wallet/v0">>,
  <<"tags">> => [ #{
    <<"name">> => <<"Payment Resources">>,
    <<"description">> => <<"Платформа предоставляет возможность токенизации исходных данных пользовательских платёжных ресурсов. См. [RBKmoney Payment Resource API](?api/payres/swagger.yaml).\n">>,
    <<"x-displayName">> => <<"Платёжные ресурсы">>
  }, #{
    <<"name">> => <<"Providers">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Провайдеры услуг">>
  }, #{
    <<"name">> => <<"Identities">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Владельцы">>
  }, #{
    <<"name">> => <<"Wallets">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Кошельки">>
  }, #{
    <<"name">> => <<"Deposits">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Пополнения">>
  }, #{
    <<"name">> => <<"Withdrawals">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Выводы">>
  }, #{
    <<"name">> => <<"Residences">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Резиденции">>
  }, #{
    <<"name">> => <<"Currencies">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Валюты">>
  }, #{
    <<"name">> => <<"Reports">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Отчеты">>
  }, #{
    <<"name">> => <<"Downloads">>,
    <<"description">> => <<"">>,
    <<"x-displayName">> => <<"Загрузка файлов">>
  }, #{
    <<"name">> => <<"P2P">>,
    <<"description">> => <<"Переводы средств между людьми с использованием инструментов вне платформы">>,
    <<"x-displayName">> => <<"Переводы вне платформы">>
  }, #{
    <<"name">> => <<"P2PTemplates">>,
    <<"description">> => <<"Шаблоны переводов вне платформы позволяют упростить создание переводов. Шаблон перевода привязан к [личности-владельцу](#tag/Identities) и содержит спецификацию, по которой можно создавать переводы, указывая только конкретнyю сумму и/или метаданные перевода.\n\nШаблоны служат в первую очередь для простой и при этом безопасной реализации сценариев совершения переводов клиентами в браузере, например с использованием нашей формы.\n### Сценарий интеграции\n1. Владелец [создаёт шаблон перевода](#operation/createP2PTransferTemplate), при помощи личного кабинета или непосредственно вызова метода API.\n2. Владелец [выписывает токен доступа](#operation/issueP2PTransferTemplateAccessToken) к шаблону и использует его вместе с идентификатором шаблона при встраивании формы для переводов на своём сайте.\n3. Будучи открытой клиентом, форма использует [полученный токен](#operation/issueP2PTransferTemplateAccessToken) для доступа к различной информации о шаблоне и для [выписывания талона](#operation/issueP2PTransferTicket) на единоразовое совершение перевода.\n4. Клиент заполняет необходимые данные, и, используя [полученный талон](#operation/issueP2PTransferTicket) в качестве токена доступа, запускает перевод и начинает следить изменениями его статуса*.\n5. В случае неуспешного завершения перевода форма может повторить процесс, [выписав очередной талон](#operation/issueP2PTransferTicket) на совершение перевода.\n* В том случае, если перевод не удалось создать, например по причине кратковременной недоступности сервиса, повторный запрос с тем же талоном будет обработан идемпотентно.\n\nCоздание и модификация шаблона перевода не требуют верификации на стороне платформы и заявок на эти изменения.\n### Токен доступа к шаблону перевода\nТокен доступа к шаблону перевода создается с помощью соответствующего метода API [issueP2PTransferTemplateAccessToken](#operation/issueP2PTransferTemplateAccessToken). Он авторизует:\n* чтение шаблона,\n* выписывание талона для проведения перевода по шаблону.\n\n### Талон на совершение перевода\nТалон на совершение перевода является расширенной версией токена доступа и создается с помощью соответствующего метода API [issueP2PTransferTicket](#operation/issueP2PTransferTicket). Он авторизует:\n* запросы комиссии перевода,\n* одну попытку создания перевода по данному шаблону,\n* операции чтения, связанные с этим переводом.\n">>,
    <<"x-displayName">> => <<"Шаблоны переводов вне платформы">>
  }, #{
    <<"name">> => <<"W2W">>,
    <<"description">> => <<"Переводы средств между кошельками внутри платформы">>,
    <<"x-displayName">> => <<"Переводы внутри платформы">>
  }, #{
    <<"name">> => <<"Webhooks">>,
    <<"description">> => <<"## RBKmoney Webhooks Management API\nВ данном разделе описаны методы, позволяющие управлять Webhook'ами, или инструментами для получения асинхронных оповещений посредством HTTP-запросов при наступлении одного или группы интересующих вас событий, например, о том, что выплата в рамках созданного кошелька была успешно проведена.\n## RBKmoney Webhooks Events API\nВнимание! Только Webhooks Management API является частью платформы RBKmoney, а следовательно и данной спецификации. Для реализации обработчика присылаемых уведомлений вам необходимо будет ознакомиться со Swagger-спецификацей [RBKmoney Wallet Webhooks Events API](https://rbkmoney.github.io/wallet-webhook-events-api/).\n">>,
    <<"x-displayName">> => <<"Webhooks">>
  }, #{
    <<"name">> => <<"Error Codes">>,
    <<"description">> => <<"\n## Ошибки перевода\n\n  | Код                    | Описание                                                                                                                                       |\n  | ---                    | --------                                                                                                                                       |\n  | InvalidSenderResource  | Неверный источник перевода (введен номер несуществующей карты, отсутствующего аккаунта и т.п.)                                                 |\n  | InvalidReceiverResource| Неверный получатель перевода (введен номер несуществующей карты и т.п.)                                                                        |\n  | InsufficientFunds      | Недостаточно средств на счете банковской карты                                                                                                 |\n  | PreauthorizationFailed | Предварительная авторизация отклонена (введен неверный код 3D-Secure, на форме 3D-Secure нажата ссылка отмены)                                 |\n  | RejectedByIssuer       | Перевод отклонён эмитентом (установлены запреты по стране списания, запрет на покупки в интернете, платёж отклонен антифродом эмитента и т.п.) |\n  | P2PTransferRejected    | Перевод отклонён по иным причинам                                                                                                              |\n">>,
    <<"x-displayName">> => <<"Коды ошибок">>
  } ],
  <<"schemes">> => [ <<"https">> ],
  <<"consumes">> => [ <<"application/json; charset=utf-8">> ],
  <<"produces">> => [ <<"application/json; charset=utf-8">> ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"paths">> => #{
    <<"/currencies/{currencyID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Currencies">> ],
        <<"summary">> => <<"Получить описание валюты">>,
        <<"operationId">> => <<"getCurrency">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Za-z]{3}$">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Валюта найдена">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Currency">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/deposits">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Deposits">> ],
        <<"summary">> => <<"Поиск пополнений">>,
        <<"operationId">> => <<"listDeposits">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор кошелька">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"depositID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор ввода денежных средств">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 50,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"sourceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор источника средств">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        }, #{
          <<"name">> => <<"createdAtFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Дата создания с">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"createdAtTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Дата создания до">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"amountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Сумма денежных средств в минорных единицах">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"amountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Сумма денежных средств в минорных единицах">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Результат поиска">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      }
    },
    <<"/destinations">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Перечислить приёмники средств">>,
        <<"operationId">> => <<"listDestinations">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Результат поиска">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_1">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Завести приёмник средств">>,
        <<"operationId">> => <<"createDestination">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"destination">>,
          <<"description">> => <<"Данные приёмника средств">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Destination">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Приёмник средств создан">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Destination">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI созданного приёмника средств">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные приёмника средств">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/destinations/{destinationID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Получить приёмник средств">>,
        <<"operationId">> => <<"getDestination">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"destinationID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор приёмника средств">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Приёмник средств найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Destination">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/destinations/{destinationID}/grants">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Выдать право управления приёмником средств">>,
        <<"operationId">> => <<"issueDestinationGrant">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"destinationID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор приёмника средств">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"request">>,
          <<"description">> => <<"Запрос на право управления приёмником средств">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/DestinationGrantRequest">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Право выдано">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DestinationGrantRequest">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для выдачи">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/external-ids/destinations/{externalID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Получить приёмник средств по внешнему идентификатору">>,
        <<"operationId">> => <<"getDestinationByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Внешний идентификатор">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Приёмник средств найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Destination">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/external-ids/withdrawals/{externalID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Получить состояние вывода средств по внешнему идентификатору">>,
        <<"operationId">> => <<"getWithdrawalByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Внешний идентификатор">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Вывод найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Withdrawal">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/external/wallets">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Получить кошелёк по указанному внешнему идентификатору">>,
        <<"operationId">> => <<"getWalletByExternalID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Внешний идентификатор кошелька">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Данные кошелька">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Wallet">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/files/{fileID}/download">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Downloads">> ],
        <<"description">> => <<"Получить ссылку для скачивания файла">>,
        <<"operationId">> => <<"downloadFile">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"fileID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор файла">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Данные для получения файла">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/FileDownload">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/identities">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Перечислить личности владельцев">>,
        <<"operationId">> => <<"listIdentities">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор провайдера услуг">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"class">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Класс личности">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"level">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уровень идентификации">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Результат поиска">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_2">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Создать личность владельца">>,
        <<"operationId">> => <<"createIdentity">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"identity">>,
          <<"description">> => <<"Данные создаваемой личности">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Identity">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Личность владельца создана">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Identity">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI созданной личности">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные личности владельца">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/identities/{identityID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Получить данные личности владельца">>,
        <<"operationId">> => <<"getIdentity">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Личность владельца найдена">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Identity">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/identities/{identityID}/challenges">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Перечислить процедуры идентификации">>,
        <<"operationId">> => <<"listIdentityChallenges">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Статус процедуры идентификации личности.\n\n| Значение    | Пояснение                       |\n| ----------- | ------------------------------- |\n| `Pending`   | Процедура в процессе выполнения |\n| `Completed` | Процедура пройдена успешно      |\n| `Failed`    | Процедура не пройдена           |\n| `Cancelled` | Процедура отменена              |\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"Pending">>, <<"Completed">>, <<"Failed">>, <<"Cancelled">> ]
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Процедуры идентификации найдены">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/IdentityChallenge">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Запустить процедуру идентификации">>,
        <<"operationId">> => <<"startIdentityChallenge">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"challenge">>,
          <<"description">> => <<"Данные для запуска процедуры идентификации">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/IdentityChallenge">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Процедура идентификации запущена">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/IdentityChallenge">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI запущенной процедуры">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"409">> => #{
            <<"description">> => <<"Ранее запущенная процедура ещё не завершена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для проведения процедуры">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/identities/{identityID}/challenges/{challengeID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Получить данные процедуры идентификации">>,
        <<"operationId">> => <<"getIdentityChallenge">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"challengeID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор процедуры идентификации личности">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Процедура идентификации найдена">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/IdentityChallenge">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/identities/{identityID}/challenges/{challengeID}/events">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Запросить события процедуры идентификации">>,
        <<"operationId">> => <<"pollIdentityChallengeEvents">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"challengeID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор процедуры идентификации личности">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"eventCursor">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор последнего известного события.\n\nВсе события, произошедшие _после_ указанного, попадут в выборку.\nЕсли этот параметр не указан, в выборку попадут события, начиная с самого первого.\n">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"События найдены">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/IdentityChallengeEvent">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/identities/{identityID}/challenges/{challengeID}/events/{eventID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Identities">> ],
        <<"summary">> => <<"Получить событие процедуры идентификации">>,
        <<"operationId">> => <<"getIdentityChallengeEvent">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"challengeID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор процедуры идентификации личности">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"eventID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор события процедуры идентификации.\n">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Событие найдено">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/IdentityChallengeEvent">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/identities/{identityID}/reports">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Получить список отчетов по личности владельца за период">>,
        <<"operationId">> => <<"getReports">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"type">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип получаемых отчетов">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"withdrawalRegistry">> ]
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденные отчеты">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Report">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Сгенерировать отчет с указанным типом по личности владельца за указанный промежуток времени">>,
        <<"operationId">> => <<"createReport">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"ReportParams">>,
          <<"description">> => <<"Параметры генерации отчета">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/ReportParams">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Отчет создан">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Report">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      }
    },
    <<"/identities/{identityID}/reports/{reportID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Получить отчет по данному идентификатору">>,
        <<"operationId">> => <<"getReport">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"reportID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор отчета">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденный отчет">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Report">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/p2p/quotes">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2P">> ],
        <<"description">> => <<"Расчитать комиссии и зафиксировать их в виде токена, который можно использовать при создании перевода\n">>,
        <<"operationId">> => <<"quoteP2PTransfer">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"quoteParams">>,
          <<"description">> => <<"Параметры для вычисления комиссий">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/QuoteParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Результат вычисления комиссий">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransferQuote">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для расчета комиссий">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfer/templates">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2PTemplates">> ],
        <<"description">> => <<"Создать шаблон перевода">>,
        <<"operationId">> => <<"createP2PTransferTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"transferTemplateParams">>,
          <<"description">> => <<"Параметры создания шаблона перевода">>,
          <<"required">> => false,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/P2PTransferTemplateParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Шаблон перевода создан">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransferTemplate">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные входные данные для создания шаблона перевода">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfer/templates/{p2pTransferTemplateID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"P2PTemplates">> ],
        <<"description">> => <<"Получить шаблон перевода по его идентификатору.">>,
        <<"operationId">> => <<"getP2PTransferTemplateByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор шаблона перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Шаблон перевода">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransferTemplate">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/p2p/transfer/templates/{p2pTransferTemplateID}/access-tokens">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2PTemplates">> ],
        <<"description">> => <<"Получить токен доступа для чтения шаблона и\nвыписывание талонов на создание переводов по шаблону\n">>,
        <<"operationId">> => <<"issueP2PTransferTemplateAccessToken">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор шаблона перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"request">>,
          <<"description">> => <<"Запрос на получение токена доступа">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/P2PTransferTemplateTokenRequest">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Токен получен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransferTemplateTokenRequest">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для получения">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfer/templates/{p2pTransferTemplateID}/block">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2PTemplates">> ],
        <<"description">> => <<"Заблокировать использование шаблона.">>,
        <<"operationId">> => <<"blockP2PTransferTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор шаблона перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Шаблон заблокирован">>
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для блокировки шаблона">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfer/templates/{p2pTransferTemplateID}/quotes">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2PTemplates">> ],
        <<"description">> => <<"Расчитать комиссии и зафиксировать их в виде токена, который можно использовать при создании перевода по шаблону\n">>,
        <<"operationId">> => <<"quoteP2PTransferWithTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор шаблона перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"quoteParams">>,
          <<"description">> => <<"Параметры для вычисления комиссий">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/P2PTransferTemplateQuoteParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Результат вычисления комиссий">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransferQuote">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для расчета комиссий">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfer/templates/{p2pTransferTemplateID}/tickets">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2PTemplates">> ],
        <<"description">> => <<"Получить талон на одну попытку создания перевода по шаблону">>,
        <<"operationId">> => <<"issueP2PTransferTicket">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор шаблона перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"request">>,
          <<"description">> => <<"Запрос на получение талона">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/P2PTransferTemplateTicketRequest">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Талон получен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransferTemplateTokenRequest">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для получения">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfer/templates/{p2pTransferTemplateID}/transfers">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2PTemplates">> ],
        <<"description">> => <<"Создать новый перевод по шаблону.">>,
        <<"operationId">> => <<"createP2PTransferWithTemplate">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferTemplateID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор шаблона перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"p2pTransferParamsWithTemplate">>,
          <<"description">> => <<"Параметры создаваемого перевода">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/P2PTransferWithTemplateParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Перевод запущен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransfer">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI запущенного перевода">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные входные данные для создания перевода">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfers">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"P2P">> ],
        <<"description">> => <<"Создать перевод">>,
        <<"operationId">> => <<"createP2PTransfer">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"transferParams">>,
          <<"description">> => <<"Параметры создания перевода">>,
          <<"required">> => false,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/P2PTransferParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Перевод запущен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransfer">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI запущенного перевода">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные входные данные для перевода">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/p2p/transfers/{p2pTransferID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"P2P">> ],
        <<"description">> => <<"Получить состояние перевода.">>,
        <<"operationId">> => <<"getP2PTransfer">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденный перевод">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/P2PTransfer">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/p2p/transfers/{p2pTransferID}/events">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"P2P">> ],
        <<"description">> => <<"Получить историю перевода в виде набора событий.">>,
        <<"operationId">> => <<"getP2PTransferEvents">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"p2pTransferID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Результат запроса событий">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_3">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/providers">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"Перечислить доступных провайдеров">>,
        <<"operationId">> => <<"listProviders">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"residence">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Резиденция, в рамках которой производится оказание услуг,\nкод страны или региона по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Za-z]{3}$">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Провайдеры найдены">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Provider">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      }
    },
    <<"/providers/{providerID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"Получить данные провайдера">>,
        <<"operationId">> => <<"getProvider">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор провайдера">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Провайдер найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Provider">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/providers/{providerID}/identity-classes">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"Перечислить классы личностей">>,
        <<"operationId">> => <<"listProviderIdentityClasses">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор провайдера">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Классы личностей найдены">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/IdentityClass">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/providers/{providerID}/identity-classes/{identityClassID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"Получить описание класса личностей">>,
        <<"operationId">> => <<"getProviderIdentityClass">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор провайдера">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityClassID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Класс личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Класс личностей найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/IdentityClass">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/providers/{providerID}/identity-classes/{identityClassID}/levels">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"Перечислить уровни идентификации личности">>,
        <<"operationId">> => <<"listProviderIdentityLevels">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор провайдера">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityClassID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Класс личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Уровни идентификации найдены">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/IdentityLevel">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/providers/{providerID}/identity-classes/{identityClassID}/levels/{identityLevelID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Providers">> ],
        <<"summary">> => <<"Получить описание уровня идентификации">>,
        <<"operationId">> => <<"getProviderIdentityLevel">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"providerID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор провайдера">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityClassID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Класс личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        }, #{
          <<"name">> => <<"identityLevelID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Уровень идентификации">>,
          <<"required">> => true,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Уровень идентификации найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/IdentityLevel">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/residences/{residence}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Residences">> ],
        <<"summary">> => <<"Получить описание региона резиденции">>,
        <<"operationId">> => <<"getResidence">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"residence">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Резиденция, в рамках которой производится оказание услуг,\nкод страны или региона по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Za-z]{3}$">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Регион резиденции найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Residence">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/w2w/transfers">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"W2W">> ],
        <<"description">> => <<"Создать перевод">>,
        <<"operationId">> => <<"createW2WTransfer">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"transferParams">>,
          <<"description">> => <<"Параметры создания перевода">>,
          <<"required">> => false,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/W2WTransferParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Перевод запущен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/W2WTransfer">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI запущенного перевода">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные входные данные для перевода">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/w2w/transfers/{w2wTransferID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"W2W">> ],
        <<"description">> => <<"Получить состояние перевода.">>,
        <<"operationId">> => <<"getW2WTransfer">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"w2wTransferID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор перевода">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденный перевод">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/W2WTransfer">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/wallets">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Перечислить кошельки">>,
        <<"operationId">> => <<"listWallets">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Результат поиска">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_4">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Завести новый кошелёк">>,
        <<"operationId">> => <<"createWallet">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"wallet">>,
          <<"description">> => <<"Данные создаваемого кошелька">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Wallet">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Кошелёк создан">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Wallet">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI созданного кошелька">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные кошелька">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/wallets/{walletID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Получить данные кошелька">>,
        <<"operationId">> => <<"getWallet">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор кошелька">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Кошелёк найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Wallet">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/wallets/{walletID}/account">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Получить состояние счёта">>,
        <<"operationId">> => <<"getWalletAccount">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор кошелька">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Счёт кошелька получен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WalletAccount">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/wallets/{walletID}/grants">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Wallets">> ],
        <<"summary">> => <<"Выдать право управления средствами">>,
        <<"operationId">> => <<"issueWalletGrant">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор кошелька">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"request">>,
          <<"description">> => <<"Запрос на право управления средствами на кошельке">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/WalletGrantRequest">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Единоразовое право выдано">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WalletGrantRequest">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для выдачи">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/webhooks">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Получить набор установленных webhook'ов.">>,
        <<"operationId">> => <<"getWebhooks">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Набор webhook'ов">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/Webhook">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для получения webhook'ов">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Установить новый webhook.">>,
        <<"operationId">> => <<"createWebhook">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"webhookParams">>,
          <<"description">> => <<"Параметры устанавливаемого webhook'а">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/Webhook">>
          }
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Webhook установлен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Webhook">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для создания webhook'а">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/webhooks/{webhookID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Получить webhook по его идентификатору.">>,
        <<"operationId">> => <<"getWebhookByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"webhookID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор webhook'а">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Данные webhook'а">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Webhook">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для получения webhook'а">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      },
      <<"delete">> => #{
        <<"tags">> => [ <<"Webhooks">> ],
        <<"description">> => <<"Снять указанный webhook.">>,
        <<"operationId">> => <<"deleteWebhookByID">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"webhookID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор webhook'а">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Webhook успешно снят">>
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для снятия webhook'а">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/withdrawal-quotes">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Подготовка котировки">>,
        <<"description">> => <<"Фиксация курса обмена валют для проведения выплаты с конвертацией">>,
        <<"operationId">> => <<"createQuote">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"withdrawalQuoteParams">>,
          <<"description">> => <<"Данные котировки для вывода">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/WithdrawalQuoteParams">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Полученная котировка">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WithdrawalQuote">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для получения котировки">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/withdrawals">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Поиск выводов">>,
        <<"operationId">> => <<"listWithdrawals">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"walletID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор кошелька">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"identityID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор личности владельца">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор вывода денежных средств">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"destinationID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор приёмника средств">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        }, #{
          <<"name">> => <<"createdAtFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Дата создания с">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"createdAtTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Дата создания до">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"amountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Сумма денежных средств в минорных единицах">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"amountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Сумма денежных средств в минорных единицах">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"currencyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Результат поиска">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_5">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Запустить вывод средств">>,
        <<"operationId">> => <<"createWithdrawal">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"in">> => <<"body">>,
          <<"name">> => <<"withdrawal">>,
          <<"description">> => <<"Данные вывода">>,
          <<"required">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/definitions/WithdrawalParameters">>
          }
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Вывод запущен">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Withdrawal">>
            },
            <<"headers">> => #{
              <<"Location">> => #{
                <<"type">> => <<"string">>,
                <<"format">> => <<"uri">>,
                <<"description">> => <<"URI запущенного вывода">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"409">> => #{
            <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ConflictRequest">>
            }
          },
          <<"422">> => #{
            <<"description">> => <<"Неверные данные для осуществления вывода">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/InvalidOperationParameters">>
            }
          }
        }
      }
    },
    <<"/withdrawals/{withdrawalID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Получить состояние вывода средств">>,
        <<"operationId">> => <<"getWithdrawal">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор вывода денежных средств">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Вывод найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Withdrawal">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/withdrawals/{withdrawalID}/events">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Запросить события вывода средств">>,
        <<"operationId">> => <<"pollWithdrawalEvents">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор вывода денежных средств">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"eventCursor">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор последнего известного события.\n\nВсе события, произошедшие _после_ указанного, попадут в выборку.\nЕсли этот параметр не указан, в выборку попадут события, начиная с самого первого.\n">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"События найдены">>,
            <<"schema">> => #{
              <<"type">> => <<"array">>,
              <<"items">> => #{
                <<"$ref">> => <<"#/definitions/WithdrawalEvent">>
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    },
    <<"/withdrawals/{withdrawalID}/events/{eventID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Withdrawals">> ],
        <<"summary">> => <<"Получить событие вывода средств">>,
        <<"operationId">> => <<"getWithdrawalEvents">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"withdrawalID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор вывода денежных средств">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"eventID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор события процедуры идентификации.\n">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Событие найдено">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/WithdrawalEvent">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Недопустимые для операции входные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/BadRequest">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Искомая сущность не найдена">>
          }
        }
      }
    }
  },
  <<"securityDefinitions">> => #{
    <<"bearer">> => #{
      <<"description">> => <<"Для аутентификации вызовов мы используем [JWT](https://jwt.io). Cоответствующий ключ передается в заголовке.\n```shell\n Authorization: Bearer {TOKENIZATION|PRIVATE_JWT}\n```\n">>,
      <<"type">> => <<"apiKey">>,
      <<"name">> => <<"Authorization">>,
      <<"in">> => <<"header">>
    }
  },
  <<"definitions">> => #{
    <<"Asset">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Объём денежных средств\n">>
    },
    <<"BankCardDestinationResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/DestinationResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      } ],
      <<"description">> => <<"Банковская карта">>
    },
    <<"BankCardReceiverResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/ReceiverResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"paymentSystem">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Платёжная система">>,
            <<"readOnly">> => true,
            <<"enum">> => [ <<"visa">>, <<"mastercard">>, <<"visaelectron">>, <<"maestro">>, <<"forbrugsforeningen">>, <<"dankort">>, <<"amex">>, <<"dinersclub">>, <<"discover">>, <<"unionpay">>, <<"jcb">>, <<"nspkmir">>, <<"elo">>, <<"rupay">>, <<"dummy">>, <<"uzcard">> ]
          }
        }
      } ],
      <<"description">> => <<"Банковская карта">>
    },
    <<"BankCardReceiverResourceParams">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/ReceiverResourceParams">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      } ],
      <<"description">> => <<"Банковская карта">>
    },
    <<"BankCardSenderResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/SenderResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"paymentSystem">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Платёжная система">>,
            <<"readOnly">> => true,
            <<"enum">> => [ <<"visa">>, <<"mastercard">>, <<"visaelectron">>, <<"maestro">>, <<"forbrugsforeningen">>, <<"dankort">>, <<"amex">>, <<"dinersclub">>, <<"discover">>, <<"unionpay">>, <<"jcb">>, <<"nspkmir">>, <<"elo">>, <<"rupay">>, <<"dummy">>, <<"uzcard">> ]
          }
        }
      } ],
      <<"description">> => <<"Банковская карта">>
    },
    <<"BankCardSenderResourceParams">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/SenderResourceParams">>
      }, #{
        <<"$ref">> => <<"#/definitions/SecuredBankCard">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"authData">> ],
        <<"properties">> => #{
          <<"authData">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Данные авторизации, полученные при сохранении карты">>,
            <<"maxLength">> => 1000
          }
        }
      } ],
      <<"description">> => <<"Банковская карта">>
    },
    <<"Bitcoin">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ]
    },
    <<"BitcoinCash">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ]
    },
    <<"BrowserGetRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/BrowserRequest">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"uriTemplate">> ],
        <<"properties">> => #{
          <<"uriTemplate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Шаблон значения URL для перехода в браузере\n\nШаблон представлен согласно стандарту\n[RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
          }
        }
      } ]
    },
    <<"BrowserPostRequest">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/BrowserRequest">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"form">>, <<"uriTemplate">> ],
        <<"properties">> => #{
          <<"uriTemplate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Шаблон значения URL для отправки формы\n\nШаблон представлен согласно стандарту\n[RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
          },
          <<"form">> => #{
            <<"$ref">> => <<"#/definitions/UserInteractionForm">>
          }
        }
      } ]
    },
    <<"BrowserRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"requestType">> ],
      <<"discriminator">> => <<"requestType">>,
      <<"properties">> => #{
        <<"requestType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип браузерной операции">>
        }
      }
    },
    <<"ContactInfo">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"email">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"email">>,
          <<"description">> => <<"Адрес электронной почты">>,
          <<"maxLength">> => 100
        },
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"^\\+\\d{4,15}$">>,
          <<"description">> => <<"Номер мобильного телефона с международным префиксом согласно\n[E.164](https://en.wikipedia.org/wiki/E.164).\n">>
        }
      },
      <<"description">> => <<"Контактные данные">>
    },
    <<"ContinuationToken">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
    },
    <<"CryptoCurrency">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Криптовалюта">>,
      <<"enum">> => [ <<"Bitcoin">>, <<"Litecoin">>, <<"BitcoinCash">>, <<"Ripple">>, <<"Ethereum">>, <<"Zcash">>, <<"USDT">> ]
    },
    <<"CryptoWallet">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"id">> ],
      <<"discriminator">> => <<"currency">>,
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"zu3TcwGI71Bpaaw2XkLWZXlhMdn4zpVzMQ">>,
          <<"description">> => <<"Идентификатор (он же адрес) криптовалютного кошелька">>,
          <<"minLength">> => 16,
          <<"maxLength">> => 256
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Криптовалюта">>,
          <<"enum">> => [ <<"Bitcoin">>, <<"Litecoin">>, <<"BitcoinCash">>, <<"Ripple">>, <<"Ethereum">>, <<"Zcash">>, <<"USDT">> ]
        }
      },
      <<"description">> => <<"Данные криптовалютного кошелька">>
    },
    <<"CryptoWalletDestinationResource">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/DestinationResource">>
      }, #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ],
      <<"description">> => <<"Криптовалютные денежные средства">>
    },
    <<"Currency">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"exponent">>, <<"id">>, <<"name">>, <<"numericCode">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"numericCode">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"643">>,
          <<"description">> => <<"Цифровой код валюты согласно\n[ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm)\n">>,
          <<"pattern">> => <<"^[0-9]{3}$">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Российский рубль">>,
          <<"description">> => <<"Человекочитаемое название валюты\n">>
        },
        <<"sign">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"₽">>,
          <<"description">> => <<"Знак единицы валюты\n">>
        },
        <<"exponent">> => #{
          <<"type">> => <<"integer">>,
          <<"example">> => 2,
          <<"description">> => <<"Количество допустимых знаков после запятой в сумме средств, в которых может\nуказываться количество минорных денежных единиц\n">>,
          <<"minimum">> => 0
        }
      },
      <<"description">> => <<"Описание валюты">>,
      <<"example">> => #{
        <<"name">> => <<"Российский рубль">>,
        <<"sign">> => <<"₽">>,
        <<"id">> => <<"RUB">>,
        <<"numericCode">> => <<"643">>,
        <<"exponent">> => 2
      }
    },
    <<"CurrencyID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
      <<"example">> => <<"RUB">>
    },
    <<"Deposit">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"body">>, <<"source">>, <<"wallet">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"tZ0jUmlsV0">>,
            <<"description">> => <<"Идентификатор поступления денежных средств">>,
            <<"readOnly">> => true
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время запуска пополнения">>,
            <<"readOnly">> => true
          },
          <<"wallet">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Идентификатор кошелька">>
          },
          <<"source">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"107498">>,
            <<"description">> => <<"Идентификатор источника денежных средств">>
          },
          <<"body">> => #{
            <<"$ref">> => <<"#/definitions/Deposit_body">>
          },
          <<"fee">> => #{
            <<"$ref">> => <<"#/definitions/Deposit_fee">>
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/DepositStatus">>
      } ],
      <<"description">> => <<"Данные поступления денежных средств">>
    },
    <<"DepositFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Код ошибки поступления">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      }
    },
    <<"DepositID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор поступления денежных средств">>,
      <<"example">> => <<"tZ0jUmlsV0">>
    },
    <<"DepositStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус поступления денежных средств.\n\n| Значение    | Пояснение                                        |\n| ----------- | ------------------------------------------------ |\n| `Pending`   | Поступление в процессе выполнения                |\n| `Succeeded` | Поступление средств произведён успешно           |\n| `Failed`    | Поступление средств завершился неудачей          |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/DepositStatus_failure">>
        }
      }
    },
    <<"Destination">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"currency">>, <<"identity">>, <<"name">>, <<"resource">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"107498">>,
            <<"description">> => <<"Идентификатор приёмника денежных средств">>,
            <<"readOnly">> => true
          },
          <<"name">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"Squarey plastic thingy">>,
            <<"description">> => <<"Человекочитаемое название приёмника средств, по которому его легко узнать\n">>
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время создания приёмника средств">>,
            <<"readOnly">> => true
          },
          <<"isBlocked">> => #{
            <<"type">> => <<"boolean">>,
            <<"example">> => false,
            <<"description">> => <<"Заблокирован ли приёмник средств?">>,
            <<"readOnly">> => true
          },
          <<"identity">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"Идентификатор личности владельца кошелька">>
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"RUB">>,
            <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"resource">> => #{
            <<"$ref">> => <<"#/definitions/DestinationResource">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"example">> => #{
              <<"color_hint">> => <<"olive-green">>
            },
            <<"description">> => <<"Произвольный, специфичный для клиента API и непрозрачный для системы набор данных, ассоциированных с\nданным приёмником\n">>,
            <<"properties">> => #{ }
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/DestinationStatus">>
      } ],
      <<"description">> => <<"Данные приёмника денежных средств">>
    },
    <<"DestinationGrantRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"validUntil">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"Токен, дающий право управления выводами">>,
          <<"readOnly">> => true,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время, до наступления которых выданное право действительно\n">>
        }
      },
      <<"description">> => <<"Запрос на право управления выводами на приёмник средств">>,
      <<"example">> => #{
        <<"validUntil">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"token">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>
      }
    },
    <<"DestinationID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор приёмника денежных средств">>,
      <<"example">> => <<"107498">>
    },
    <<"DestinationResource">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип ресурса приёмника средств.\n\nСм. [RBKmoney Payment Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardDestinationResource">>, <<"CryptoWalletDestinationResource">> ]
        }
      },
      <<"description">> => <<"Ресурс приёмника денежных средств, используемый для осуществления выводов">>
    },
    <<"DestinationStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Authorized">>,
          <<"description">> => <<"Статус приёмника денежных средств.\n\n| Значение       | Пояснение                                  |\n| -------------- | ------------------------------------------ |\n| `Unauthorized` | Не авторизован владельцем на вывод средств |\n| `Authorized`   | Авторизован владельцем на вывод средств    |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Unauthorized">>, <<"Authorized">> ]
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"> Если `status` == `Authorized`\n\nДата и время, до наступления которых авторизация действительна\n">>,
          <<"readOnly">> => true
        }
      }
    },
    <<"DestinationsTopic">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WebhookScope">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"eventTypes">> ],
        <<"properties">> => #{
          <<"eventTypes">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"Набор типов событий приёмника денежных средств, о которых следует оповещать">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"enum">> => [ <<"DestinationCreated">>, <<"DestinationUnauthorized">>, <<"DestinationAuthorized">> ]
            }
          }
        }
      } ],
      <<"description">> => <<"Область охвата, включающая события по приёмникам денежных средств\nв рамках определённого кошелька\n">>
    },
    <<"InvalidOperationParameters">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"No such identity challenge type: fms.\n">>
        }
      },
      <<"description">> => <<"Неверные входные данные для операции">>
    },
    <<"Ethereum">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ]
    },
    <<"ExternalID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>,
      <<"example">> => <<"10036274">>
    },
    <<"FileDownload">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"expiresAt">>, <<"url">> ],
      <<"properties">> => #{
        <<"url">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"URL файла">>
        },
        <<"expiresAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Время до которого ссылка будет считаться действительной">>
        }
      },
      <<"example">> => #{
        <<"url">> => <<"url">>,
        <<"expiresAt">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"GrantToken">> => #{
      <<"type">> => <<"string">>,
      <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\\n">>
    },
    <<"Identity">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"class">>, <<"name">>, <<"provider">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца кошелька">>,
          <<"readOnly">> => true
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Keyn Fawkes">>,
          <<"description">> => <<"Человекочитаемое имя личности владельца, по которому его легко опознать\n">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания личности владельца">>,
          <<"readOnly">> => true
        },
        <<"provider">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"ncoeps">>,
          <<"description">> => <<"Идентификатор провайдера услуг">>
        },
        <<"class">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"person">>,
          <<"description">> => <<"Класс личности владельцев">>
        },
        <<"level">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"partial">>,
          <<"description">> => <<"Уровень идентификации личности">>,
          <<"readOnly">> => true
        },
        <<"effectiveChallenge">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"1">>,
          <<"description">> => <<"Идентификатор успешно пройденной процедура идентификации, результат которой действителен на\nтекущий момент\n">>,
          <<"readOnly">> => true
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>,
          <<"example">> => false,
          <<"description">> => <<"Заблокирована ли личность владельца?">>,
          <<"readOnly">> => true
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"example">> => #{
            <<"lkDisplayName">> => <<"Сергей Иванович">>
          },
          <<"description">> => <<"Произвольный, специфичный для клиента API и непрозрачный для системы набор данных, ассоциированных с\nданной личностью владельца\n">>,
          <<"properties">> => #{ }
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        },
        <<"partyID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Уникальный в рамках платформы идентификатор участника.">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        }
      },
      <<"description">> => <<"Данные личности владельца кошельков">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"effectiveChallenge">> => <<"1">>,
        <<"metadata">> => #{
          <<"lkDisplayName">> => <<"Сергей Иванович">>
        },
        <<"provider">> => <<"ncoeps">>,
        <<"level">> => <<"partial">>,
        <<"name">> => <<"Keyn Fawkes">>,
        <<"isBlocked">> => false,
        <<"externalID">> => <<"10036274">>,
        <<"id">> => <<"10036274">>,
        <<"partyID">> => <<"partyID">>,
        <<"class">> => <<"person">>
      }
    },
    <<"IdentityChallenge">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"proofs">>, <<"type">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"1">>,
            <<"description">> => <<"Идентификатор процедуры идентификации личности">>,
            <<"readOnly">> => true
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время запуска процедуры">>,
            <<"readOnly">> => true
          },
          <<"type">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"esia">>,
            <<"description">> => <<"Тип процедуры идентификации личности">>
          },
          <<"proofs">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"Набор представленных доказательств.\n\nСм. [RBKmoney Private Documents API](?api/privdoc/swagger.json).\n">>,
            <<"uniqueItems">> => true,
            <<"items">> => #{
              <<"$ref">> => <<"#/definitions/SecuredPrivateDocument">>
            }
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/IdentityChallengeStatus">>
      } ],
      <<"description">> => <<"Процедура идентификации личности">>
    },
    <<"IdentityChallengeEvent">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changes">>, <<"eventID">>, <<"occuredAt">> ],
      <<"properties">> => #{
        <<"eventID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"example">> => 42,
          <<"description">> => <<"Идентификатор события процедуры идентификации">>
        },
        <<"occuredAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время возникновения события">>
        },
        <<"changes">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/IdentityChallengeEventChange">>
          }
        }
      },
      <<"description">> => <<"Событие, возникшее в процессе процедуры идентификации\n">>,
      <<"example">> => #{
        <<"eventID">> => 42,
        <<"occuredAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"changes">> => [ #{
          <<"type">> => <<"IdentityChallengeStatusChanged">>
        }, #{
          <<"type">> => <<"IdentityChallengeStatusChanged">>
        } ]
      }
    },
    <<"IdentityChallengeEventChange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип произошедшего изменения">>,
          <<"enum">> => [ <<"IdentityChallengeStatusChanged">> ]
        }
      },
      <<"description">> => <<"Изменение, возникшее в процессе процедуры идентификации\n">>,
      <<"example">> => #{
        <<"type">> => <<"IdentityChallengeStatusChanged">>
      }
    },
    <<"IdentityChallengeID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор процедуры идентификации личности">>,
      <<"example">> => <<"1">>
    },
    <<"IdentityChallengeStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус процедуры идентификации личности.\n\n| Значение    | Пояснение                       |\n| ----------- | ------------------------------- |\n| `Pending`   | Процедура в процессе выполнения |\n| `Completed` | Процедура пройдена успешно      |\n| `Failed`    | Процедура не пройдена           |\n| `Cancelled` | Процедура отменена              |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Pending">>, <<"Completed">>, <<"Failed">>, <<"Cancelled">> ]
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"> Если `status` == `Completed`\n\nДата и время, до наступления которых результат процедуры действителен\n">>,
          <<"readOnly">> => true
        },
        <<"failureReason">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Proof incomplete">>,
          <<"description">> => <<"> Если `status` == `Failed`\n\nОписание причины неудачного завершения процедуры\n">>,
          <<"readOnly">> => true
        }
      }
    },
    <<"IdentityChallengeStatusChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/IdentityChallengeEventChange">>
      }, #{
        <<"$ref">> => <<"#/definitions/IdentityChallengeStatus">>
      } ],
      <<"description">> => <<"Изменение статуса процедуры идентификации">>
    },
    <<"IdentityChallengeStatusValue">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Статус процедуры идентификации личности.\n\n| Значение    | Пояснение                       |\n| ----------- | ------------------------------- |\n| `Pending`   | Процедура в процессе выполнения |\n| `Completed` | Процедура пройдена успешно      |\n| `Failed`    | Процедура не пройдена           |\n| `Cancelled` | Процедура отменена              |\n">>,
      <<"enum">> => [ <<"Pending">>, <<"Completed">>, <<"Failed">>, <<"Cancelled">> ]
    },
    <<"IdentityChallengeType">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"esia">>,
          <<"description">> => <<"Тип процедуры идентификации личности">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Упрощённая идентификация">>,
          <<"description">> => <<"Человекочитаемое название процедуры идентификации\n">>
        },
        <<"requiredProofs">> => #{
          <<"type">> => <<"array">>,
          <<"example">> => [ <<"RUSDomesticPassport">>, <<"RUSRetireeInsuranceCertificate">> ],
          <<"description">> => <<"Набор доказательств, которые необходимо предоставить\n">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Тип документа">>,
            <<"enum">> => [ <<"RUSDomesticPassport">>, <<"RUSRetireeInsuranceCertificate">> ]
          }
        }
      },
      <<"description">> => <<"Описание типа процедуры идентификации личности">>,
      <<"example">> => #{
        <<"name">> => <<"Упрощённая идентификация">>,
        <<"id">> => <<"esia">>,
        <<"requiredProofs">> => [ <<"RUSDomesticPassport">>, <<"RUSRetireeInsuranceCertificate">> ]
      }
    },
    <<"IdentityChallengeTypeID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Тип процедуры идентификации личности">>,
      <<"example">> => <<"esia">>
    },
    <<"IdentityClass">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"person">>,
          <<"description">> => <<"Класс личности владельцев">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Частное лицо">>,
          <<"description">> => <<"Человекочитаемое название класса\n">>
        }
      },
      <<"example">> => #{
        <<"name">> => <<"Частное лицо">>,
        <<"id">> => <<"person">>
      }
    },
    <<"IdentityClassID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Класс личности владельцев">>,
      <<"example">> => <<"person">>
    },
    <<"IdentityID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор личности владельца кошелька">>,
      <<"example">> => <<"10036274">>
    },
    <<"IdentityLevel">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"partial">>,
          <<"description">> => <<"Уровень идентификации личности">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Частично идентифицирован">>,
          <<"description">> => <<"Человекочитаемое название уровня идентификации\n">>
        },
        <<"challenges">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Список возможных процедур, которые необходимо пройти для достижения этого\nуровня\n">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/IdentityChallengeType">>
          }
        }
      },
      <<"example">> => #{
        <<"challenges">> => [ #{
          <<"name">> => <<"Упрощённая идентификация">>,
          <<"id">> => <<"esia">>,
          <<"requiredProofs">> => [ <<"RUSDomesticPassport">>, <<"RUSRetireeInsuranceCertificate">> ]
        }, #{
          <<"name">> => <<"Упрощённая идентификация">>,
          <<"id">> => <<"esia">>,
          <<"requiredProofs">> => [ <<"RUSDomesticPassport">>, <<"RUSRetireeInsuranceCertificate">> ]
        } ],
        <<"name">> => <<"Частично идентифицирован">>,
        <<"id">> => <<"partial">>
      }
    },
    <<"IdentityLevelID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Уровень идентификации личности">>,
      <<"example">> => <<"partial">>
    },
    <<"BadRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"errorType">> ],
      <<"properties">> => #{
        <<"errorType">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"NotFound">>,
          <<"description">> => <<"Тип ошибки в данных">>,
          <<"enum">> => [ <<"SchemaViolated">>, <<"NotFound">>, <<"WrongType">>, <<"NotInRange">>, <<"WrongSize">>, <<"WrongLength">>, <<"WrongArray">>, <<"NoMatch">>, <<"InvalidResourceToken">>, <<"InvalidToken">>, <<"AmbiguousPartyID">> ]
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"X-Request-ID">>,
          <<"description">> => <<"Имя или идентификатор элемента сообщения, содержащего недопустимые данные">>
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Required parameter was not sent">>,
          <<"description">> => <<"Пояснение, почему данные считаются недопустимыми">>
        }
      }
    },
    <<"ConflictRequest">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Переданное значение `externalID`, для которого обнаружен конфликт параметров запроса">>
        },
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор сущности, созданной предыдущим запросом с указанным `externalID`">>
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Человекочитаемое описание ошибки">>
        }
      }
    },
    <<"BankCardPaymentSystem">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Платёжная система">>,
      <<"enum">> => [ <<"visa">>, <<"mastercard">>, <<"visaelectron">>, <<"maestro">>, <<"forbrugsforeningen">>, <<"dankort">>, <<"amex">>, <<"dinersclub">>, <<"discover">>, <<"unionpay">>, <<"jcb">>, <<"nspkmir">>, <<"elo">>, <<"rupay">>, <<"dummy">>, <<"uzcard">> ]
    },
    <<"SecuredBankCard">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"token">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"zu3TcwGI71Bpaaw2XkLWZXlhMdn4zpVzMQg9xMkh">>,
          <<"description">> => <<"Токен, идентифицирующий исходные данные карты">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 1000
        },
        <<"bin">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"424242">>,
          <<"description">> => <<"[Идентификационный номер][1] банка-эмитента карты\n\n[1]: https://en.wikipedia.org/wiki/Payment_card_number#Issuer_identification_number_(IIN)\n">>,
          <<"readOnly">> => true,
          <<"pattern">> => <<"^\\d{6,8}$">>
        },
        <<"lastDigits">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"4242">>,
          <<"description">> => <<"Последние цифры номера карты">>,
          <<"readOnly">> => true,
          <<"pattern">> => <<"^\\d{2,4}$">>
        }
      },
      <<"description">> => <<"Безопасные данные банковской карты">>
    },
    <<"RUSDomesticPassport">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/SecuredPrivateDocument">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"seriesMasked">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"45**">>,
            <<"description">> => <<"Маскированная серия паспорта">>,
            <<"readOnly">> => true
          },
          <<"numberMasked">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"*****6">>,
            <<"description">> => <<"Маскированный номер паспорта">>,
            <<"readOnly">> => true
          },
          <<"fullnameMasked">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"И***** И.И.">>,
            <<"description">> => <<"Маскированное полное имя гражданина">>,
            <<"readOnly">> => true
          }
        }
      } ],
      <<"description">> => <<"Открытые данные национального паспорта гражданина РФ">>
    },
    <<"RUSRetireeInsuranceCertificate">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/SecuredPrivateDocument">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"numberMasked">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"12*-***-*** *4">>,
            <<"description">> => <<"Маскированный [СНИЛС](https://ru.wikipedia.org/wiki/Страховой_номер_индивидуального_лицевого_счёта)\n">>,
            <<"readOnly">> => true
          }
        }
      } ],
      <<"description">> => <<"Открытые данные страхового свидетельства обязательного пенсионного страхования\nгражданина РФ\n">>
    },
    <<"SecuredPrivateDocument">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"token">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип документа">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"RUSDomesticPassport">>, <<"RUSRetireeInsuranceCertificate">> ]
        },
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"7kodCm5gr68ji4O11KpVn0">>,
          <<"description">> => <<"Токен, уникально идентифицирующий исходные данные">>
        }
      },
      <<"description">> => <<"Персональный документ, токенизированный с целью сокрытия чувствительных данных\n">>
    },
    <<"SecuredPrivateDocumentType">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Тип документа">>,
      <<"enum">> => [ <<"RUSDomesticPassport">>, <<"RUSRetireeInsuranceCertificate">> ]
    },
    <<"Litecoin">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ]
    },
    <<"P2PTransfer">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"contactInfo">>, <<"createdAt">>, <<"id">>, <<"identityID">>, <<"receiver">>, <<"sender">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10a0b68D3E21">>,
          <<"description">> => <<"Идентификатор перевода">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца перевода">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransfer_body">>
        },
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/SenderResource">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/ReceiverResource">>
        },
        <<"status">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferStatus">>
        },
        <<"contactInfo">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransfer_contactInfo">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Связанные с переводом метаданные">>,
          <<"properties">> => #{ }
        }
      },
      <<"description">> => <<"Данные перевода">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"metadata">> => <<"{}">>,
        <<"receiver">> => #{
          <<"type">> => <<"BankCardReceiverResource">>
        },
        <<"contactInfo">> => #{
          <<"phoneNumber">> => <<"phoneNumber">>,
          <<"email">> => <<"email">>
        },
        <<"sender">> => #{
          <<"type">> => <<"BankCardSenderResource">>
        },
        <<"identityID">> => <<"10036274">>,
        <<"externalID">> => <<"10036274">>,
        <<"id">> => <<"10a0b68D3E21">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"status">> => #{
          <<"failure">> => #{
            <<"code">> => <<"code">>,
            <<"subError">> => #{
              <<"code">> => <<"code">>,
              <<"subError">> => null
            }
          },
          <<"status">> => <<"Pending">>
        }
      }
    },
    <<"P2PTransferChange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changeType">> ],
      <<"discriminator">> => <<"changeType">>,
      <<"properties">> => #{
        <<"changeType">> => #{
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"P2PTransferStatusChanged">>, <<"P2PTransferInteractionChanged">> ]
        }
      },
      <<"example">> => #{
        <<"changeType">> => <<"P2PTransferStatusChanged">>
      }
    },
    <<"P2PTransferEvent">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"change">>, <<"createdAt">> ],
      <<"properties">> => #{
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        },
        <<"change">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferChange">>
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"change">> => #{
          <<"changeType">> => <<"P2PTransferStatusChanged">>
        }
      }
    },
    <<"P2PTransferFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Основной код ошибки">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"[Ошибка, возникшая в процессе проведения перевода](#tag/Error-Codes)\n">>
    },
    <<"P2PTransferID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор перевода">>,
      <<"example">> => <<"10a0b68D3E21">>
    },
    <<"P2PTransferInteractionChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/P2PTransferChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"userInteractionChange">>, <<"userInteractionID">> ],
        <<"properties">> => #{
          <<"userInteractionID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор взаимодействия с пользователем">>
          },
          <<"userInteractionChange">> => #{
            <<"$ref">> => <<"#/definitions/UserInteractionChange">>
          }
        },
        <<"description">> => <<"Изменения определённого взаимодействия с пользователем\n">>
      } ]
    },
    <<"P2PTransferParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"contactInfo">>, <<"identityID">>, <<"receiver">>, <<"sender">> ],
      <<"properties">> => #{
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/SenderResourceParams">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/ReceiverResourceParams">>
        },
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца перевода">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferParameters_body">>
        },
        <<"contactInfo">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransfer_contactInfo">>
        },
        <<"quoteToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, полученный в результате вычисления и фиксирования комиссий перевода\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Метаданные, которые необходимо связать с переводом">>,
          <<"properties">> => #{ }
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        }
      },
      <<"description">> => <<"Параметры создания перевода">>,
      <<"example">> => #{
        <<"metadata">> => <<"{}">>,
        <<"receiver">> => #{
          <<"type">> => <<"BankCardReceiverResourceParams">>
        },
        <<"contactInfo">> => #{
          <<"phoneNumber">> => <<"phoneNumber">>,
          <<"email">> => <<"email">>
        },
        <<"sender">> => #{
          <<"type">> => <<"BankCardSenderResourceParams">>
        },
        <<"identityID">> => <<"10036274">>,
        <<"quoteToken">> => <<"quoteToken">>,
        <<"externalID">> => <<"10036274">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        }
      }
    },
    <<"P2PTransferQuote">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"customerFee">>, <<"expiresOn">>, <<"token">> ],
      <<"properties">> => #{
        <<"customerFee">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferQuote_customerFee">>
        },
        <<"token">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferQuoteToken">>
        },
        <<"expiresOn">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время прекращения действия токена">>
        }
      },
      <<"description">> => <<"Комиссия и токен предназначеный для создания перевода">>,
      <<"example">> => #{
        <<"customerFee">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"expiresOn">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"token">> => #{ }
      }
    },
    <<"P2PTransferQuoteToken">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Токен, полученный в результате вычисления и фиксирования комиссий перевода\n">>
    },
    <<"P2PTransferStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус вывода денежных средств.\n\n| Значение    | Пояснение                                  |\n| ----------- | ------------------------------------------ |\n| `Pending`   | Перевод в процессе выполнения                |\n| `Succeeded` | Перевод средств произведён успешно           |\n| `Failed`    | Перевод средств завершился неудачей          |\n">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferStatus_failure">>
        }
      },
      <<"example">> => #{
        <<"failure">> => #{
          <<"code">> => <<"code">>,
          <<"subError">> => #{
            <<"code">> => <<"code">>,
            <<"subError">> => null
          }
        },
        <<"status">> => <<"Pending">>
      }
    },
    <<"P2PTransferStatusChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/P2PTransferChange">>
      }, #{
        <<"$ref">> => <<"#/definitions/P2PTransferStatus">>
      } ]
    },
    <<"P2PTransferTemplate">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"createdAt">>, <<"details">>, <<"id">>, <<"identityID">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10a0b68D3E21">>,
          <<"description">> => <<"Идентификатор шаблона перевода">>,
          <<"readOnly">> => true,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца шаблона">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>,
          <<"example">> => false,
          <<"description">> => <<"Заблокирован ли шаблон?">>,
          <<"readOnly">> => true
        },
        <<"details">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferTemplateDetails">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"identityID">> => <<"10036274">>,
        <<"isBlocked">> => false,
        <<"externalID">> => <<"10036274">>,
        <<"details">> => #{
          <<"metadata">> => #{
            <<"defaultMetadata">> => <<"{}">>
          },
          <<"description">> => <<"description">>,
          <<"body">> => #{
            <<"value">> => #{
              <<"amount">> => 1430000,
              <<"currency">> => <<"RUB">>
            }
          }
        },
        <<"id">> => <<"10a0b68D3E21">>
      }
    },
    <<"P2PTransferTemplateBody">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"value">> ],
      <<"properties">> => #{
        <<"value">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferTemplateBody_value">>
        }
      },
      <<"example">> => #{
        <<"value">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        }
      }
    },
    <<"P2PTransferTemplateDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">> ],
      <<"properties">> => #{
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Описание перевода">>,
          <<"maxLength">> => 1000
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferTemplateBody">>
        },
        <<"metadata">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferTemplateMetadata">>
        }
      },
      <<"description">> => <<"Параметры, использующиеся при создании перевода">>,
      <<"example">> => #{
        <<"metadata">> => #{
          <<"defaultMetadata">> => <<"{}">>
        },
        <<"description">> => <<"description">>,
        <<"body">> => #{
          <<"value">> => #{
            <<"amount">> => 1430000,
            <<"currency">> => <<"RUB">>
          }
        }
      }
    },
    <<"P2PTransferTemplateID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор шаблона перевода">>,
      <<"example">> => <<"10a0b68D3E21">>
    },
    <<"P2PTransferTemplateMetadata">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"defaultMetadata">> ],
      <<"properties">> => #{
        <<"defaultMetadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Метаданные, которые будут связаны с переводом, созданным по шаблону. Непосредственно при создании перевода можно передать дополнительные метаданные. При совпадении ключей предпочтение будет отдаваться значениям из этого поля шаблона.\n">>,
          <<"properties">> => #{ }
        }
      },
      <<"example">> => #{
        <<"defaultMetadata">> => <<"{}">>
      }
    },
    <<"P2PTransferTemplateParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"details">>, <<"identityID">> ],
      <<"properties">> => #{
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца шаблона">>
        },
        <<"details">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferTemplateDetails">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        }
      },
      <<"example">> => #{
        <<"identityID">> => <<"10036274">>,
        <<"externalID">> => <<"10036274">>,
        <<"details">> => #{
          <<"metadata">> => #{
            <<"defaultMetadata">> => <<"{}">>
          },
          <<"description">> => <<"description">>,
          <<"body">> => #{
            <<"value">> => #{
              <<"amount">> => 1430000,
              <<"currency">> => <<"RUB">>
            }
          }
        }
      }
    },
    <<"P2PTransferTemplateQuoteParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"receiver">>, <<"sender">> ],
      <<"properties">> => #{
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/SenderResource">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/ReceiverResource">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransfer_body">>
        }
      },
      <<"description">> => <<"Параметры запроса комиссий">>,
      <<"example">> => #{
        <<"receiver">> => #{
          <<"type">> => <<"BankCardReceiverResource">>
        },
        <<"sender">> => #{
          <<"type">> => <<"BankCardSenderResource">>
        },
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        }
      }
    },
    <<"P2PTransferTemplateTicketRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"validUntil">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"readOnly">> => true,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Желаемые дата и время, до наступления которых будет действителен выданный токен.\nИтоговое значение может быть ограничено сроком действия AccessToken, его можно узнать из ответа.\n">>
        }
      },
      <<"example">> => #{
        <<"validUntil">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"token">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>
      }
    },
    <<"P2PTransferTemplateTokenRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"validUntil">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"readOnly">> => true,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время, до наступления которых выданный токен действителен\n">>
        }
      },
      <<"example">> => #{
        <<"validUntil">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"token">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>
      }
    },
    <<"P2PTransferWithTemplateParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"contactInfo">>, <<"receiver">>, <<"sender">> ],
      <<"properties">> => #{
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/SenderResourceParams">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/ReceiverResourceParams">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferParameters_body">>
        },
        <<"contactInfo">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransfer_contactInfo">>
        },
        <<"quoteToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, полученный в результате вычисления и фиксирования комиссий перевода\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"description">> => <<"Метаданные, которые необходимо связать с переводом.\nДобавляются к указанным при создании шаблона.\nПри совпадении ключей предпочтение отдается значениям из шаблона.\n">>,
          <<"properties">> => #{ }
        }
      },
      <<"example">> => #{
        <<"metadata">> => <<"{}">>,
        <<"receiver">> => #{
          <<"type">> => <<"BankCardReceiverResourceParams">>
        },
        <<"contactInfo">> => #{
          <<"phoneNumber">> => <<"phoneNumber">>,
          <<"email">> => <<"email">>
        },
        <<"sender">> => #{
          <<"type">> => <<"BankCardSenderResourceParams">>
        },
        <<"quoteToken">> => <<"quoteToken">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        }
      }
    },
    <<"PartyID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Уникальный в рамках платформы идентификатор участника.">>
    },
    <<"Provider">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"name">>, <<"residences">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"ncoeps">>,
          <<"description">> => <<"Идентификатор провайдера услуг">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"НКО «ЭПС»">>,
          <<"description">> => <<"Человекочитаемое наименование провайдера услуг\n">>
        },
        <<"residences">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Резиденции, в которых провайдер предоставляет услуги\n">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"RUS">>,
            <<"description">> => <<"Резиденция, символьный код по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          }
        }
      },
      <<"description">> => <<"Данные провайдера услуг">>,
      <<"example">> => #{
        <<"name">> => <<"НКО «ЭПС»">>,
        <<"id">> => <<"ncoeps">>,
        <<"residences">> => [ <<"RUS">>, <<"RUS">> ]
      }
    },
    <<"ProviderID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор провайдера услуг">>,
      <<"example">> => <<"ncoeps">>
    },
    <<"QuoteParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"identityID">>, <<"receiver">>, <<"sender">> ],
      <<"properties">> => #{
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/SenderResource">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/ReceiverResource">>
        },
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца кошелька">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransfer_body">>
        }
      },
      <<"description">> => <<"Параметры запроса комиссий">>,
      <<"example">> => #{
        <<"receiver">> => #{
          <<"type">> => <<"BankCardReceiverResource">>
        },
        <<"sender">> => #{
          <<"type">> => <<"BankCardSenderResource">>
        },
        <<"identityID">> => <<"10036274">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        }
      }
    },
    <<"ReceiverResource">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип ресурса получателя средств.\n\nСм. [RBKmoney Payment Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardReceiverResource">> ]
        }
      },
      <<"description">> => <<"Ресурс получателя денежных средств, используемый для осуществления переводов">>,
      <<"example">> => #{
        <<"type">> => <<"BankCardReceiverResource">>
      }
    },
    <<"ReceiverResourceParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип ресурса получателя средств.\n\nСм. [RBKmoney Payment Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardReceiverResourceParams">> ]
        }
      },
      <<"description">> => <<"Параметры ресурса получателя денежных средств">>,
      <<"example">> => #{
        <<"type">> => <<"BankCardReceiverResourceParams">>
      }
    },
    <<"Redirect">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteraction">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"request">> ],
        <<"properties">> => #{
          <<"request">> => #{
            <<"$ref">> => <<"#/definitions/BrowserRequest">>
          }
        }
      } ]
    },
    <<"Report">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"createdAt">>, <<"files">>, <<"fromTime">>, <<"id">>, <<"status">>, <<"toTime">>, <<"type">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Идентификатор отчета">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"fromTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время начала периода">>
        },
        <<"toTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время конца периода">>
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус формирования отчета">>,
          <<"enum">> => [ <<"pending">>, <<"created">>, <<"canceled">> ]
        },
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип отчета">>,
          <<"enum">> => [ <<"withdrawalRegistry">> ]
        },
        <<"files">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Report_files">>
          }
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"fromTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"files">> => [ #{
          <<"id">> => <<"id">>
        }, #{
          <<"id">> => <<"id">>
        } ],
        <<"id">> => 0,
        <<"type">> => <<"withdrawalRegistry">>,
        <<"toTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"status">> => <<"pending">>
      }
    },
    <<"ReportParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"fromTime">>, <<"reportType">>, <<"toTime">> ],
      <<"properties">> => #{
        <<"reportType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип отчета">>,
          <<"enum">> => [ <<"withdrawalRegistry">> ]
        },
        <<"fromTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Начало временного отрезка">>
        },
        <<"toTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Конец временного отрезка">>
        }
      },
      <<"example">> => #{
        <<"reportType">> => <<"withdrawalRegistry">>,
        <<"fromTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"toTime">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"Residence">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">>, <<"name">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUS">>,
          <<"description">> => <<"Резиденция, символьный код по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Российская федерация">>,
          <<"description">> => <<"Человекочитаемое название региона резиденции\n">>
        },
        <<"flag">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"🇷🇺">>,
          <<"description">> => <<"Флаг региона резиденции\n">>
        }
      },
      <<"description">> => <<"Описание региона резиденции">>,
      <<"example">> => #{
        <<"flag">> => <<"🇷🇺">>,
        <<"name">> => <<"Российская федерация">>,
        <<"id">> => <<"RUS">>
      }
    },
    <<"ResidenceID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Резиденция, символьный код по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
      <<"example">> => <<"RUS">>
    },
    <<"Ripple">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"tag">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"191919191">>,
            <<"description">> => <<"Уникальный идентификатор учетной записи Ripple (XRP)">>,
            <<"pattern">> => <<"^[0-9]{9}$">>
          }
        }
      } ]
    },
    <<"SenderResource">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип ресурса отправителя средств.\n\nСм. [RBKmoney Payment Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardSenderResource">> ]
        }
      },
      <<"description">> => <<"Ресурс отправителя денежных средств, используемый для осуществления переводов">>,
      <<"example">> => #{
        <<"type">> => <<"BankCardSenderResource">>
      }
    },
    <<"SenderResourceParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип ресурса отправителя средств.\n\nСм. [RBKmoney Payment Resource API](?api/payres/swagger.yaml).\n">>,
          <<"enum">> => [ <<"BankCardSenderResourceParams">> ]
        }
      },
      <<"description">> => <<"Параметры ресурса отправителя денежных средств">>,
      <<"example">> => #{
        <<"type">> => <<"BankCardSenderResourceParams">>
      }
    },
    <<"SourceID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор источника денежных средств">>,
      <<"example">> => <<"107498">>
    },
    <<"SubFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Детализация кода ошибки">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"Детализация описания ошибки\n">>,
      <<"example">> => #{
        <<"code">> => <<"code">>,
        <<"subError">> => null
      }
    },
    <<"USDT">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ]
    },
    <<"UserInteraction">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"interactionType">> ],
      <<"discriminator">> => <<"interactionType">>,
      <<"properties">> => #{
        <<"interactionType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип взаимодействия с пользователем">>
        }
      }
    },
    <<"UserInteractionChange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changeType">> ],
      <<"discriminator">> => <<"changeType">>,
      <<"properties">> => #{
        <<"changeType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Вид изменения взаимодействию с пользователем.">>,
          <<"enum">> => [ <<"UserInteractionCreated">>, <<"UserInteractionFinished">> ]
        }
      }
    },
    <<"UserInteractionCreated">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteractionChange">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"userInteraction">> ],
        <<"properties">> => #{
          <<"userInteraction">> => #{
            <<"$ref">> => <<"#/definitions/UserInteraction">>
          }
        }
      } ]
    },
    <<"UserInteractionFinished">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/UserInteractionChange">>
      } ]
    },
    <<"UserInteractionForm">> => #{
      <<"type">> => <<"array">>,
      <<"description">> => <<"Форма для отправки средствами браузера">>,
      <<"items">> => #{
        <<"$ref">> => <<"#/definitions/UserInteractionForm_inner">>
      }
    },
    <<"W2WTransfer">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"createdAt">>, <<"id">>, <<"receiver">>, <<"sender">>, <<"status">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10a0b68D3E21">>,
          <<"description">> => <<"Идентификатор перевода">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransfer_body">>
        },
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"status">> => #{
          <<"$ref">> => <<"#/definitions/W2WTransferStatus">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        }
      },
      <<"description">> => <<"Данные перевода">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"receiver">> => <<"10068321">>,
        <<"sender">> => <<"10068321">>,
        <<"externalID">> => <<"10036274">>,
        <<"id">> => <<"10a0b68D3E21">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"status">> => #{
          <<"failure">> => #{
            <<"code">> => <<"code">>,
            <<"subError">> => #{
              <<"code">> => <<"code">>,
              <<"subError">> => null
            }
          },
          <<"status">> => <<"Pending">>
        }
      }
    },
    <<"W2WTransferFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Основной код ошибки">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"[Ошибка, возникшая в процессе проведения перевода](#tag/Error-Codes)\n">>
    },
    <<"W2WTransferID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор перевода">>,
      <<"example">> => <<"10a0b68D3E21">>
    },
    <<"W2WTransferParameters">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"body">>, <<"receiver">>, <<"sender">> ],
      <<"properties">> => #{
        <<"sender">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"receiver">> => #{
          <<"$ref">> => <<"#/definitions/WalletID">>
        },
        <<"body">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferParameters_body">>
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        }
      },
      <<"description">> => <<"Параметры создания перевода">>,
      <<"example">> => #{
        <<"receiver">> => <<"10068321">>,
        <<"sender">> => <<"10068321">>,
        <<"externalID">> => <<"10036274">>,
        <<"body">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        }
      }
    },
    <<"W2WTransferStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус перевода денежных средств.\n\n| Значение    | Пояснение                                  |\n| ----------- | ------------------------------------------ |\n| `Pending`   | Перевод в процессе выполнения                |\n| `Succeeded` | Перевод средств произведён успешно           |\n| `Failed`    | Перевод средств завершился неудачей          |\n">>,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/P2PTransferStatus_failure">>
        }
      },
      <<"example">> => #{
        <<"failure">> => #{
          <<"code">> => <<"code">>,
          <<"subError">> => #{
            <<"code">> => <<"code">>,
            <<"subError">> => null
          }
        },
        <<"status">> => <<"Pending">>
      }
    },
    <<"Wallet">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"identity">>, <<"name">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10068321">>,
          <<"description">> => <<"Идентификатор кошелька">>,
          <<"readOnly">> => true
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Worldwide PHP Awareness Initiative">>,
          <<"description">> => <<"Человекочитаемое название кошелька, по которому его легко узнать">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания кошелька">>,
          <<"readOnly">> => true
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>,
          <<"example">> => false,
          <<"description">> => <<"Заблокирован ли кошелёк?">>,
          <<"readOnly">> => true
        },
        <<"identity">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца кошелька">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"example">> => #{
            <<"client_locale">> => <<"RU_ru">>
          },
          <<"description">> => <<"Произвольный, специфичный для клиента API и непрозрачный для системы набор данных, ассоциированных с\nданным кошельком\n">>,
          <<"properties">> => #{ }
        },
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        }
      },
      <<"description">> => <<"Данные кошелька">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"metadata">> => #{
          <<"client_locale">> => <<"RU_ru">>
        },
        <<"identity">> => <<"10036274">>,
        <<"name">> => <<"Worldwide PHP Awareness Initiative">>,
        <<"isBlocked">> => false,
        <<"externalID">> => <<"10036274">>,
        <<"currency">> => <<"RUB">>,
        <<"id">> => <<"10068321">>
      }
    },
    <<"WalletAccount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"available">>, <<"own">> ],
      <<"properties">> => #{
        <<"own">> => #{
          <<"$ref">> => <<"#/definitions/WalletAccount_own">>
        },
        <<"available">> => #{
          <<"$ref">> => <<"#/definitions/WalletAccount_available">>
        }
      },
      <<"description">> => <<"Состояние счёта кошелька">>,
      <<"example">> => #{
        <<"own">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"available">> => <<"{\"amount\":1200000,\"currency\":\"RUB\"}">>
      }
    },
    <<"WalletGrantRequest">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"asset">>, <<"validUntil">> ],
      <<"properties">> => #{
        <<"token">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"Токен, дающий право единоразового управления средствами на кошельке">>,
          <<"readOnly">> => true,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"asset">> => #{
          <<"$ref">> => <<"#/definitions/WalletGrantRequest_asset">>
        },
        <<"validUntil">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время, до наступления которых выданное право действительно\n">>
        }
      },
      <<"description">> => <<"Запрос на единоразовое право управления средствами на кошельке">>,
      <<"example">> => #{
        <<"validUntil">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"asset">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"token">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>
      }
    },
    <<"WalletID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор кошелька">>,
      <<"example">> => <<"10068321">>
    },
    <<"WalletName">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Человекочитаемое название кошелька, по которому его легко узнать">>,
      <<"example">> => <<"Worldwide PHP Awareness Initiative">>
    },
    <<"Webhook">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"identityID">>, <<"scope">>, <<"url">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор webhook'а\n">>,
          <<"readOnly">> => true
        },
        <<"identityID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Идентификатор личности владельца кошелька">>
        },
        <<"active">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"Включена ли в данный момент доставка оповещений?\n">>,
          <<"readOnly">> => true
        },
        <<"scope">> => #{
          <<"$ref">> => <<"#/definitions/WebhookScope">>
        },
        <<"url">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"uri">>,
          <<"description">> => <<"URL, на который будут поступать оповещения о произошедших событиях\n">>,
          <<"maxLength">> => 1000
        },
        <<"publicKey">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"hexadecimal">>,
          <<"example">> => <<"MIGJAoGBAM1fmNUvezts3yglTdhXuqG7OhHxQtDFA+Ss//YuUGjw5ossDbEMoS+SIFuYZ/UL9Xg0rEHNRSbmf48OK+mz0FobEtbji8MADayzGfFopXsfRFa7MVy3Uhu5jBDpLsN3DyJapAkK0TAYINlZXxVjDwxRNheTvC+xub5WNdiwc28fAgMBAAE=">>,
          <<"description">> => <<"Содержимое публичного ключа, служащего для проверки авторитативности\nприходящих на `url` оповещений\n">>,
          <<"readOnly">> => true
        }
      },
      <<"example">> => #{
        <<"identityID">> => <<"10036274">>,
        <<"scope">> => #{
          <<"topic">> => <<"WithdrawalsTopic">>
        },
        <<"active">> => true,
        <<"id">> => <<"id">>,
        <<"publicKey">> => <<"MIGJAoGBAM1fmNUvezts3yglTdhXuqG7OhHxQtDFA+Ss//YuUGjw5ossDbEMoS+SIFuYZ/UL9Xg0rEHNRSbmf48OK+mz0FobEtbji8MADayzGfFopXsfRFa7MVy3Uhu5jBDpLsN3DyJapAkK0TAYINlZXxVjDwxRNheTvC+xub5WNdiwc28fAgMBAAE=">>,
        <<"url">> => <<"http://example.com/aeiou">>
      }
    },
    <<"WebhookScope">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"topic">> ],
      <<"discriminator">> => <<"topic">>,
      <<"properties">> => #{
        <<"topic">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Предмет оповещений">>,
          <<"enum">> => [ <<"WithdrawalsTopic">>, <<"DestinationsTopic">> ]
        }
      },
      <<"description">> => <<"Область охвата webhook'а, ограничивающая набор типов событий, по которым\nследует отправлять оповещения\n">>,
      <<"example">> => #{
        <<"topic">> => <<"WithdrawalsTopic">>
      }
    },
    <<"Withdrawal">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"body">>, <<"destination">>, <<"wallet">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"tZ0jUmlsV0">>,
            <<"description">> => <<"Идентификатор вывода денежных средств">>,
            <<"readOnly">> => true
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время запуска вывода">>,
            <<"readOnly">> => true
          },
          <<"wallet">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Идентификатор кошелька">>
          },
          <<"destination">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"107498">>,
            <<"description">> => <<"Идентификатор приёмника денежных средств">>
          },
          <<"body">> => #{
            <<"$ref">> => <<"#/definitions/Withdrawal_body">>
          },
          <<"fee">> => #{
            <<"$ref">> => <<"#/definitions/Deposit_fee">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"example">> => #{
              <<"notify_email">> => <<"iliketrains@example.com">>
            },
            <<"description">> => <<"Произвольный, специфичный для клиента API и непрозрачный для системы набор данных, ассоциированных с\nданным выводом\n">>,
            <<"properties">> => #{ }
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10036274">>,
            <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/WithdrawalStatus">>
      } ],
      <<"description">> => <<"Данные вывода денежных средств">>
    },
    <<"WithdrawalEvent">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"changes">>, <<"eventID">>, <<"occuredAt">> ],
      <<"properties">> => #{
        <<"eventID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"example">> => 42,
          <<"description">> => <<"Идентификатор события вывода средств">>
        },
        <<"occuredAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время возникновения события">>
        },
        <<"changes">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/WithdrawalEventChange">>
          }
        }
      },
      <<"description">> => <<"Событие, возникшее в процессе вывода средств\n">>,
      <<"example">> => #{
        <<"eventID">> => 42,
        <<"occuredAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"changes">> => [ #{
          <<"type">> => <<"WithdrawalStatusChanged">>
        }, #{
          <<"type">> => <<"WithdrawalStatusChanged">>
        } ]
      }
    },
    <<"WithdrawalEventChange">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип произошедшего изменения">>,
          <<"enum">> => [ <<"WithdrawalStatusChanged">> ]
        }
      },
      <<"description">> => <<"Изменение, возникшее в процессе вывода средств\n">>,
      <<"example">> => #{
        <<"type">> => <<"WithdrawalStatusChanged">>
      }
    },
    <<"WithdrawalFailure">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Код ошибки вывода">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      }
    },
    <<"WithdrawalID">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Идентификатор вывода денежных средств">>,
      <<"example">> => <<"tZ0jUmlsV0">>
    },
    <<"WithdrawalParameters">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Withdrawal">>
      }, #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"walletGrant">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
            <<"description">> => <<"Токен, дающий право на списание с кошелька для оплаты вывода.\n\nНеобходимо предоставить в том случае, если оплата производится засчёт средств _чужого_\nкошелька. Владелец указанного кошелька может\n[выдать на это право](#operation/issueWalletGrant).\n">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 4000
          },
          <<"destinationGrant">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
            <<"description">> => <<"Токен, дающий право вывода.\n\nНеобходимо предоставить в том случае, если вывод производится посредством _чужого_ приёмника\nсредств. Владелец указанного приёмника может\n[выдать на это право](#operation/issueDestinationGrant).\n">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 4000
          },
          <<"quoteToken">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
            <<"description">> => <<"Котировка, по которой следует проводить вывод средств.\n\nДолжна быть [получена](#operation/createQuote)\nзаранее для каждой отдельной операции вывода с конвертацией.\n">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 4000
          }
        }
      } ],
      <<"description">> => <<"Параметры создаваемого вывода">>
    },
    <<"WithdrawalQuote">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"cashFrom">>, <<"cashTo">>, <<"createdAt">>, <<"expiresOn">>, <<"quoteToken">> ],
      <<"properties">> => #{
        <<"cashFrom">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalQuote_cashFrom">>
        },
        <<"cashTo">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalQuote_cashTo">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время получения котировки">>,
          <<"readOnly">> => true
        },
        <<"expiresOn">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время окончания действия котировки">>,
          <<"readOnly">> => true
        },
        <<"quoteToken">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"Котировка, по которой следует проводить вывод средств.\nНеобходимо предоставить при создании вывода с конвертацией\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        }
      },
      <<"description">> => <<"Данные котировки для вывода">>,
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"cashTo">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"cashFrom">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"quoteToken">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
        <<"expiresOn">> => <<"2000-01-23T04:56:07.000+00:00">>
      }
    },
    <<"WithdrawalQuoteParams">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"cash">>, <<"currencyFrom">>, <<"currencyTo">>, <<"walletID">> ],
      <<"properties">> => #{
        <<"externalID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10036274">>,
          <<"description">> => <<"Уникальный идентификатор сущности на вашей стороне.\n\nПри указании будет использован для того, чтобы гарантировать идемпотентную обработку операции.\n">>
        },
        <<"walletID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"10068321">>,
          <<"description">> => <<"Идентификатор кошелька">>
        },
        <<"destinationID">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"107498">>,
          <<"description">> => <<"Идентификатор приёмника денежных средств">>
        },
        <<"currencyFrom">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Код исходной валюты">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"currencyTo">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Код конечной валюты">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"cash">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalQuoteParams_cash">>
        },
        <<"walletGrant">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"Токен, дающий право на списание с кошелька для оплаты вывода.\nНеобходимо предоставить в том случае, если оплата производится засчёт средств _чужого_ кошелька. Владелец указанного кошелька может [выдать на это право](#operation/issueWalletGrant)\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        },
        <<"destinationGrant">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
          <<"description">> => <<"Токен, дающий право вывода.\nНеобходимо предоставить в том случае, если вывод производится посредством _чужого_ приёмника средств. Владелец указанного приёмника может [выдать на это право](#operation/issueDestinationGrant)\n">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 4000
        }
      },
      <<"description">> => <<"Параметры котировки для вывода">>,
      <<"example">> => #{
        <<"walletID">> => <<"10068321">>,
        <<"walletGrant">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
        <<"externalID">> => <<"10036274">>,
        <<"destinationGrant">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5M\nDIyfQ.XbPfbIHMI6arZ3Y922BhjWgQzWXcXNrz0ogtVhfEd2o\n">>,
        <<"destinationID">> => <<"107498">>,
        <<"currencyTo">> => <<"RUB">>,
        <<"cash">> => #{
          <<"amount">> => 1430000,
          <<"currency">> => <<"RUB">>
        },
        <<"currencyFrom">> => <<"RUB">>
      }
    },
    <<"WithdrawalStatus">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус вывода денежных средств.\n\n| Значение    | Пояснение                                  |\n| ----------- | ------------------------------------------ |\n| `Pending`   | Вывод в процессе выполнения                |\n| `Succeeded` | Вывод средств произведён успешно           |\n| `Failed`    | Вывод средств завершился неудачей          |\n">>,
          <<"readOnly">> => true,
          <<"enum">> => [ <<"Pending">>, <<"Succeeded">>, <<"Failed">> ]
        },
        <<"failure">> => #{
          <<"$ref">> => <<"#/definitions/WithdrawalStatus_failure">>
        }
      }
    },
    <<"WithdrawalStatusChanged">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WithdrawalEventChange">>
      }, #{
        <<"$ref">> => <<"#/definitions/WithdrawalStatus">>
      } ],
      <<"description">> => <<"Изменение статуса вывода средств">>
    },
    <<"WithdrawalsTopic">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/WebhookScope">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"eventTypes">> ],
        <<"properties">> => #{
          <<"walletID">> => #{
            <<"type">> => <<"string">>,
            <<"example">> => <<"10068321">>,
            <<"description">> => <<"Идентификатор кошелька">>
          },
          <<"eventTypes">> => #{
            <<"type">> => <<"array">>,
            <<"description">> => <<"Набор типов событий выплаты, о которых следует оповещать">>,
            <<"items">> => #{
              <<"type">> => <<"string">>,
              <<"enum">> => [ <<"WithdrawalStarted">>, <<"WithdrawalSucceeded">>, <<"WithdrawalFailed">> ]
            }
          }
        }
      } ],
      <<"description">> => <<"Область охвата, включающая события по выплатам в рамках определённого кошелька\n">>
    },
    <<"Zcash">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/CryptoWallet">>
      } ]
    },
    <<"inline_response_200">> => #{
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Найденные пополнения">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Deposit">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_1">> => #{
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Найденные приёмники средств">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Destination">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_2">> => #{
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Найденные личности">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Identity">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"effectiveChallenge">> => <<"1">>,
          <<"metadata">> => #{
            <<"lkDisplayName">> => <<"Сергей Иванович">>
          },
          <<"provider">> => <<"ncoeps">>,
          <<"level">> => <<"partial">>,
          <<"name">> => <<"Keyn Fawkes">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"id">> => <<"10036274">>,
          <<"partyID">> => <<"partyID">>,
          <<"class">> => <<"person">>
        }, #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"effectiveChallenge">> => <<"1">>,
          <<"metadata">> => #{
            <<"lkDisplayName">> => <<"Сергей Иванович">>
          },
          <<"provider">> => <<"ncoeps">>,
          <<"level">> => <<"partial">>,
          <<"name">> => <<"Keyn Fawkes">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"id">> => <<"10036274">>,
          <<"partyID">> => <<"partyID">>,
          <<"class">> => <<"person">>
        } ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_3">> => #{
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Набор событий">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/P2PTransferEvent">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"change">> => #{
            <<"changeType">> => <<"P2PTransferStatusChanged">>
          }
        }, #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"change">> => #{
            <<"changeType">> => <<"P2PTransferStatusChanged">>
          }
        } ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_4">> => #{
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Найденные кошельки">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Wallet">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => #{
            <<"client_locale">> => <<"RU_ru">>
          },
          <<"identity">> => <<"10036274">>,
          <<"name">> => <<"Worldwide PHP Awareness Initiative">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"currency">> => <<"RUB">>,
          <<"id">> => <<"10068321">>
        }, #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => #{
            <<"client_locale">> => <<"RU_ru">>
          },
          <<"identity">> => <<"10036274">>,
          <<"name">> => <<"Worldwide PHP Awareness Initiative">>,
          <<"isBlocked">> => false,
          <<"externalID">> => <<"10036274">>,
          <<"currency">> => <<"RUB">>,
          <<"id">> => <<"10068321">>
        } ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_5">> => #{
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"description">> => <<"Найденные выводы">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Withdrawal">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"Deposit_body">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Объём поступивших средств">>
    },
    <<"Deposit_fee">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Сумма коммисии">>
    },
    <<"DepositStatus_failure">> => #{
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Код ошибки поступления">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"> Если `status` == `Failed`\n\nПояснение причины неудачи\n">>
    },
    <<"P2PTransfer_body">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Сумма операции">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"P2PTransfer_contactInfo">> => #{
      <<"properties">> => #{
        <<"email">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"email">>,
          <<"description">> => <<"Адрес электронной почты">>,
          <<"maxLength">> => 100
        },
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"^\\+\\d{4,15}$">>,
          <<"description">> => <<"Номер мобильного телефона с международным префиксом согласно\n[E.164](https://en.wikipedia.org/wiki/E.164).\n">>
        }
      },
      <<"description">> => <<"Контактные данные отправителя">>,
      <<"example">> => #{
        <<"phoneNumber">> => <<"phoneNumber">>,
        <<"email">> => <<"email">>
      }
    },
    <<"P2PTransferParameters_body">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Сумма перевода">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"P2PTransferQuote_customerFee">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Комиссия \"сверху\" - взимается с клиента в дополнение к сумме операции">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"P2PTransferStatus_failure">> => #{
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Основной код ошибки">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"[Ошибка, возникшая в процессе проведения перевода](#tag/Error-Codes)\n">>,
      <<"example">> => #{
        <<"code">> => <<"code">>,
        <<"subError">> => #{
          <<"code">> => <<"code">>,
          <<"subError">> => null
        }
      }
    },
    <<"P2PTransferTemplateBody_value">> => #{
      <<"required">> => [ <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Объём денежных средств\n">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"Report_files">> => #{
      <<"required">> => [ <<"id">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор файла">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        }
      },
      <<"example">> => #{
        <<"id">> => <<"id">>
      }
    },
    <<"UserInteractionForm_inner">> => #{
      <<"required">> => [ <<"key">>, <<"template">> ],
      <<"properties">> => #{
        <<"key">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Значение ключа элемента формы, которую необходимо отправить средствами\nбраузера\n">>
        },
        <<"template">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Шаблон значения элемента формы\nШаблон представлен согласно стандарту\n[RFC6570](https://tools.ietf.org/html/rfc6570).\n">>
        }
      }
    },
    <<"WalletAccount_own">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Собственные средства\n">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"WalletAccount_available">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Доступные к использованию средства, обычно равны собственным средствам\nза вычетом сумм всех незавершённых операций\n">>,
      <<"example">> => <<"{\"amount\":1200000,\"currency\":\"RUB\"}">>
    },
    <<"WalletGrantRequest_asset">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Допустимый к использованию объём средств">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"Withdrawal_body">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Объём средств, которые необходимо вывести">>
    },
    <<"WithdrawalQuote_cashFrom">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Объём средств в исходной валюте">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"WithdrawalQuote_cashTo">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Объём средств в конечной валюте">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"WithdrawalQuoteParams_cash">> => #{
      <<"required">> => [ <<"amount">>, <<"currency">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"example">> => 1430000,
          <<"description">> => <<"Сумма денежных средств в минорных единицах, например, в копейках\n">>
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUB">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        }
      },
      <<"description">> => <<"Объём средств для получения котировки в одной из валют обмена">>,
      <<"example">> => #{
        <<"amount">> => 1430000,
        <<"currency">> => <<"RUB">>
      }
    },
    <<"WithdrawalStatus_failure">> => #{
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Код ошибки вывода">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubFailure">>
        }
      },
      <<"description">> => <<"> Если `status` == `Failed`\n\nПояснение причины неудачи\n">>
    }
  },
  <<"parameters">> => #{
    <<"requestID">> => #{
      <<"name">> => <<"X-Request-ID">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 32,
      <<"minLength">> => 1
    },
    <<"providerID">> => #{
      <<"name">> => <<"providerID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор провайдера">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"identityID">> => #{
      <<"name">> => <<"identityID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор личности владельца">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"challengeID">> => #{
      <<"name">> => <<"challengeID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор процедуры идентификации личности">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"walletID">> => #{
      <<"name">> => <<"walletID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор кошелька">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"destinationID">> => #{
      <<"name">> => <<"destinationID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор приёмника средств">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"withdrawalID">> => #{
      <<"name">> => <<"withdrawalID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор вывода денежных средств">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"externalID">> => #{
      <<"name">> => <<"externalID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Внешний идентификатор">>,
      <<"required">> => true,
      <<"type">> => <<"string">>
    },
    <<"residence">> => #{
      <<"name">> => <<"residence">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Резиденция, в рамках которой производится оказание услуг,\nкод страны или региона по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Za-z]{3}$">>
    },
    <<"amountFrom">> => #{
      <<"name">> => <<"amountFrom">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Сумма денежных средств в минорных единицах">>,
      <<"required">> => false,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"amountTo">> => #{
      <<"name">> => <<"amountTo">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Сумма денежных средств в минорных единицах">>,
      <<"required">> => false,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"currencyID">> => #{
      <<"name">> => <<"currencyID">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Валюта, символьный код согласно [ISO\n4217](http://www.iso.org/iso/home/standards/currency_codes.htm).\n">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>
    },
    <<"limit">> => #{
      <<"name">> => <<"limit">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Лимит выборки">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"maximum">> => 1000,
      <<"minimum">> => 1,
      <<"format">> => <<"int32">>
    },
    <<"eventCursor">> => #{
      <<"name">> => <<"eventCursor">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Идентификатор последнего известного события.\n\nВсе события, произошедшие _после_ указанного, попадут в выборку.\nЕсли этот параметр не указан, в выборку попадут события, начиная с самого первого.\n">>,
      <<"required">> => false,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int32">>
    },
    <<"eventID">> => #{
      <<"name">> => <<"eventID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор события процедуры идентификации.\n">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int32">>
    },
    <<"reportID">> => #{
      <<"name">> => <<"reportID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор отчета">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"fileID">> => #{
      <<"name">> => <<"fileID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор файла">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"fromTime">> => #{
      <<"name">> => <<"fromTime">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Начало временного отрезка">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"format">> => <<"date-time">>
    },
    <<"toTime">> => #{
      <<"name">> => <<"toTime">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Конец временного отрезка">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"format">> => <<"date-time">>
    },
    <<"deadline">> => #{
      <<"name">> => <<"X-Request-Deadline">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Максимальное время обработки запроса">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"webhookID">> => #{
      <<"name">> => <<"webhookID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор webhook'а">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"queryIdentityID">> => #{
      <<"name">> => <<"identityID">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Идентификатор личности владельца">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"p2pTransferID">> => #{
      <<"name">> => <<"p2pTransferID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор перевода">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"p2pTransferTemplateID">> => #{
      <<"name">> => <<"p2pTransferTemplateID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор шаблона перевода">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"w2wTransferID">> => #{
      <<"name">> => <<"w2wTransferID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор перевода">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    }
  },
  <<"responses">> => #{
    <<"BadRequest">> => #{
      <<"description">> => <<"Недопустимые для операции входные данные">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/BadRequest">>
      }
    },
    <<"ConflictRequest">> => #{
      <<"description">> => <<"Переданное значение `externalID` уже использовалось вами ранее с другими параметрами запроса">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/ConflictRequest">>
      }
    },
    <<"NotFound">> => #{
      <<"description">> => <<"Искомая сущность не найдена">>
    },
    <<"Unauthorized">> => #{
      <<"description">> => <<"Ошибка авторизации">>
    }
  }
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA,
  <<"{\"definitions\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": \"petType\",
         \"properties\": {
            \"name\":    {\"type\": \"string\"},
            \"petType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Person\": {
         \"type\":          \"object\",
         \"discriminator\": \"personType\",
         \"properties\": {
           \"name\": {\"type\": \"string\"},
           \"sex\": {
             \"type\": \"string\",
             \"enum\": [\"male\", \"female\"]
           },
           \"personType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"sex\", \"personType\"]
       },
       \"WildMix\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {\"$ref\": \"#/definitions/Person\"}
         ],
       },
       \"Dummy\": {
         \"type\":          \"object\",
         \"discriminator\": \"dummyType\",
         \"properties\": {
           \"name\":      {\"type\": \"string\"},
           \"dummyType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"dummyType\"]
       }
     }}">>).

get_enum(Parent, Discr, Schema) ->
    lists:sort(get_sub_schema([<<"enum">>, Discr, <<"properties">>, Parent, ?DEFINITIONS], Schema)).

-spec test() -> _.
-spec enumerate_discriminator_children_test() -> _.
enumerate_discriminator_children_test() ->
    Schema      = jsx:decode(?SCHEMA, [return_maps]),
    FixedSchema = enumerate_discriminator_children(Schema),
    ?assertEqual(lists:sort([<<"Dog">>, <<"Cat">>, <<"WildMix">>]), get_enum(<<"Pet">>, <<"petType">>, FixedSchema)),
    ?assertEqual([<<"WildMix">>], get_enum(<<"Person">>,  <<"personType">>, FixedSchema)),
    ?assertEqual([],              get_enum(<<"Dummy">>,   <<"dummyType">>,  FixedSchema)).

-spec get_test() -> _.
get_test() ->
    ?assertEqual(
       enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw())),
       ?MODULE:get()
    ).
-endif.
