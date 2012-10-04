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

-include("of_config.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Tests -----------------------------------------------------------------------

parser_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Decoding OF-Config 1.1 in Xmerl record representation "
       "to internal record representation",
       fun decode/0},
      {"Encoding internal record representation to Xmerl simple form",
       fun encode/0}]}.

decode() ->
    {XML, _Rest} = xmerl_scan:file("../test/full-config-example-1.1.xml"),
    Res = of_config:decode(XML),
    ?assertEqual(true, is_record(Res, capable_switch)).

encode() ->
    {XML1, _Rest} = xmerl_scan:file("../test/full-config-example-1.1.xml"),
    CapableSwitchRecord1 = of_config:decode(XML1),
    ?assertEqual(true, is_record(CapableSwitchRecord1, capable_switch)),

    SimpleForm = of_config:encode(CapableSwitchRecord1),
    DeepList = xmerl:export_simple(SimpleForm, xmerl_xml, [{prolog, ""}]),
    XMLString = lists:flatten(DeepList),
    {XML2, _Rest} = xmerl_scan:string(XMLString),
    CapableSwitchRecord2 = of_config:decode(XML2),
    ?assertEqual(true, is_record(CapableSwitchRecord2, capable_switch)),

    ?assertEqual(CapableSwitchRecord1, CapableSwitchRecord2).

%% Fixtures --------------------------------------------------------------------

setup() ->
    error_logger:tty(true),
    application:set_env(of_config, of_config_schema, "of-config-1.1.xsd"),

    %% HACK: Rebar is not preserving the directory structure and copies
    %%       everything to .eunit, so this symlink makes code:priv_dir/1
    %%       and include_lib work again.
    file:make_symlink("..", "of_config"),
    code:add_path("./of_config/ebin"),
    
    application:load(of_config),
    application:start(xmerl),
    application:start(of_config).

teardown(_) ->
    application:stop(of_config),
    application:stop(xmerl),
    file:delete("of_config").
    
