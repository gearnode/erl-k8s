%% Copyright (c) 2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

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
                      | {exec_error, mhttp:status(), binary()}
                      | {exec_error, mhttp:status(),
                         k8s_model:apimachinery_apis_meta_v1_status()}
                      | {invalid_exec_message, binary()}.
