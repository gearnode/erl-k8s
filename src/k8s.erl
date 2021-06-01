-module(k8s).

-export_type([result/0, result/1, error_reason/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() :: {unknown_context, k8s_config:context_name()}
                      | {request_error, term()} % TODO mhttp:error_reason()
                      | {request_error, mhttp:status(),
                         k8s_model:apimachinery_apis_meta_v1_status()}
                      | empty_response_body
                      | {invalid_response_body, mhttp:status(),
                         {invalid_json_data, json:error()} |
                         {invalid_resource_data, [jsv:value_error()]}}
                      | {invalid_exec_message, binary()}.
