%%------------------------------------------------------------------------------
%% Copyright 2012 FlowForwarding.org
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @author Konrad Kaplita <konrad.kaplita@erlang-solutions.com>
%% @author Krzysztof Rutka <krzysztof.rutka@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org
%% @doc Module for parsing and encoding OF-Config 1.1 XML configurations.
-module(of_config).

%% API
-export([decode/1,
         encode/1,
         get_version/0]).

-include("of_config.hrl").

-type of_config_version() :: '1.1' | '1.1.1'.

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec decode(xmerl_scan:xmlElement()) -> ok.
decode(XML) ->
    Schemas = lists:map(fun({Namespace, Filename}) ->
                                Path = filename:join([code:priv_dir(of_config),
                                                      Filename]),
                                {Namespace, Path}
                        end, get_schemas(get_version())),
    {ok, ProcessedSchemas} = xmerl_xsd:process_schemas(Schemas),
    case xmerl_xsd:validate(XML, ProcessedSchemas) of
        {error, _} = Error ->
            Error;
        {ValidElement, _GLobalState} ->
            of_config_decoder:to_capable_switch(ValidElement)
    end.

-spec encode(#capable_switch{}) -> of_config_encoder:simple_form().
encode(Config) ->
    of_config_encoder:to_simple_form(Config).

-spec get_version() -> of_config_version().
get_version() ->
    {ok, Version} = application:get_env(of_config, version),
    Version.

-spec get_schemas(of_config_version()) -> list(tuple(string(), string())).
get_schemas(Version) ->
    {ok, Schemas} = application:get_env(of_config, schemas),
    {Version, List} = lists:keyfind(Version, 1, Schemas),
    List.
