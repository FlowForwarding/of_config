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
%% @copyright 2012 FlowForwarding.org
%% @doc eUnit suite for testing OF-Config protocol.
%% @private
-module(of_config_tests).

-include_lib("of_config/include/of_config.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Tests -----------------------------------------------------------------------

parse_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Parsing OF-Config 1.1 XML", fun parse/0}]}.

parse() ->
    {XML, _Rest} = xmerl_scan:file("../test/full-config-example.xml"),
    Res = of_config:parse(XML),
    ?assertEqual(true, is_record(Res, capable_switch)).

%% Fixtures --------------------------------------------------------------------

setup() ->
    error_logger:tty(true),
    application:set_env(of_config, of_config_schema, "of-config-1.1.xsd"),
    application:load(of_config),
    application:start(xmerl),
    application:start(of_config).

teardown(_) ->
    application:stop(of_config),
    application:stop(xmerl).
    
