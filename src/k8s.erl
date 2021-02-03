-module(k8s).

-export_type([result/0, result/1, error_reason/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() :: {unknown_context, k8s_config:context_name()}
                      | {request_error, term()} % TODO mhttp:error_reason()
                      | empty_response_body
                      | {invalid_json_data, json:error()}
                      | {invalid_resource_data, [jsv:value_error()]}.
