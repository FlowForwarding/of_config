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

-define(XML_PATH(XML), "../test/xml/" ++ XML).

%% Tests -----------------------------------------------------------------------

%% parser_11_test_() ->
%%     {setup,
%%      fun setup_11/0,
%%      fun teardown/1,
%%      [
%%       {"Decoding/encoding full-config-1.1.xml with OF-Config 1.1 XSD",
%%        fun full_config_11/0},
%%       {"Decoding/encoding example1-edit-config-1.1.xml with OF-Config 1.1 XSD",
%%        fun example1_edit_config_11/0},
%%       {"Decoding/encoding example2-edit-config-1.1.xml with OF-Config 1.1 XSD",
%%        fun example2_edit_config_11/0},
%%       {"Decoding/encoding example3-edit-config-1.1.xml with OF-Config 1.1 XSD",
%%        fun example3_edit_config_11/0},
%%       {"Decoding/encoding get-config-1.1.xml with OF-Config 1.1 XSD",
%%        fun get_config_11/0},
%%       {"Decoding/encoding delete-controller-1.1.xml with OF-Config 1.1 XSD",
%%        fun delete_controller_11/0},
%%       {"Decoding/encoding delete-certificate-1.1.xml with OF-Config 1.1 XSD",
%%        fun delete_certificate_11/0},
%%       {"Decoding/encoding set-queue-1.1.xml with OF-Config 1.1 XSD",
%%        fun set_queue_11/0},
%%       {"Decoding/encoding set-port-1.1.xml with OF-Config 1.1 XSD",
%%        fun set_port_11/0},
%%       {"Encoding of_config_fixtures:get_config/0 fixture with OF-Config 1.1 XSD",
%%        fun encode_fixture1_11/0}
%%      ]}.

parser_111_test_() ->
    {setup,
     fun setup_111/0,
     fun teardown/1,
     [
      {"Decoding/encoding full-config-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun full_config_111/0},
      {"Decoding/encoding example1-edit-config-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun example1_edit_config_111/0},
      {"decoding/encoding example2-edit-config-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun example2_edit_config_111/0},
      {"Decoding/encoding example3-edit-config-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun example3_edit_config_111/0},
      {"Decoding/encoding get-config-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun get_config_111/0},
      {"Decoding/encoding delete-controller-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun delete_controller_111/0},
      {"Decoding/encoding delete-certificate-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun delete_certificate_111/0},
      {"Decoding/encoding set-queue-1.1.1.xml with OF-Config 1.1.1 XSD",
       fun set_queue_111/0},
      %% {"Decoding/encoding set-port-1.1.1.xml with OF-Config 1.1.1 XSD",
      %%  fun set_port_111/0},
      {"Encoding of_config_fixtures:get_config/0 fixture with OF-Config 1.1.1 XSD",
       fun encode_fixture1_111/0}
     ]}.

%% OF-Config 1.1 ---------------------------------------------------------------

full_config_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("full-config-1.1.xml")),
    encode(CapableSwitch, XML).

example1_edit_config_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("example1-edit-config-1.1.1.xml")),
    encode(CapableSwitch, XML).

example2_edit_config_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("example2-edit-config-1.1.xml")),
    encode(CapableSwitch, XML).

example3_edit_config_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("example3-edit-config-1.1.xml")),
    encode(CapableSwitch, XML).

get_config_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("get-config-1.1.xml")),
    encode(CapableSwitch, XML).

delete_controller_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("delete-controller-1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"delete\""}]).

delete_certificate_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("delete-certificate-1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"delete\""}]).

set_queue_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("set-queue-1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"replace\""}]).

set_port_11() ->
    {CapableSwitch, XML} = decode(?XML_PATH("set-port-1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"replace\""},
                                {original, "operation=\"delete\""}]).

encode_fixture1_11() ->
    CapableSwitch = of_config_fixtures:get_config(),
    encode(CapableSwitch).

%% OF-Config 1.1.1 -------------------------------------------------------------

full_config_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("full-config-1.1.1.xml")),
    encode(CapableSwitch, XML).

example1_edit_config_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("example1-edit-config-1.1.1.xml")),
    encode(CapableSwitch, XML).

example2_edit_config_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("example2-edit-config-1.1.1.xml")),
    encode(CapableSwitch, XML).

example3_edit_config_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("example3-edit-config-1.1.1.xml")),
    encode(CapableSwitch, XML).

get_config_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("get-config-1.1.1.xml")),
    encode(CapableSwitch, XML).

delete_controller_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("delete-controller-1.1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"delete\""}]).

delete_certificate_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("delete-certificate-1.1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"delete\""}]).

set_queue_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("set-queue-1.1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"replace\""}]).

set_port_111() ->
    {CapableSwitch, XML} = decode(?XML_PATH("set-port-1.1.1.xml")),
    encode(CapableSwitch, XML, [{original, "operation=\"replace\""},
                                {original, "operation=\"delete\""}]).

encode_fixture1_111() ->
    CapableSwitch = of_config_fixtures:get_config(),
    encode(CapableSwitch).

%% Helper functions ------------------------------------------------------------

decode(Filename) ->
    {XML, _Rest} = xmerl_scan:file(Filename),
    CapableSwitchRecord = of_config:decode(XML),
    ?assertEqual(true, is_record(CapableSwitchRecord, capable_switch)),
    {CapableSwitchRecord, XML}.

encode(CapableSwitch) ->
    encode(CapableSwitch, xml, false, [nodiff]).

encode(CapableSwitch, XML) ->
    encode(CapableSwitch, XML, true, [nodiff]).

encode(CapableSwitch, XML, Diff) ->
    encode(CapableSwitch, XML, true, Diff).

encode(CapableSwitchRecord1, XML, CompareWithOriginal, Diffs) ->
    SimpleForm = of_config:encode(CapableSwitchRecord1),
    DeepList = xmerl:export_simple([SimpleForm], xmerl_xml, [{prolog, ""}]),
    XMLString = lists:flatten(DeepList),
    case CompareWithOriginal of
        true ->
            %% Get rid of whitespace characters to compare XML documents
            OriginalXMLString = lists:flatten(xmerl:export([XML], xmerl_xml,
                                                           [{prolog, ""}])),
            OriginalXMLString2 = re:replace(OriginalXMLString, "\\n", "",
                                            [{return, list}, global]),
            OriginalXMLString3 = re:replace(OriginalXMLString2, " ", "",
                                            [{return, list}, global]),
            XMLString2 = re:replace(XMLString, " ", "",
                                    [{return, list}, global]),
            {OriginalFinalXML, ResultFinalXML} =
                lists:foldl(
                  fun({original, ReplaceString}, {Original, Result}) ->
                          Original2 = re:replace(Original,
                                                 ReplaceString, "",
                                                 [{return, list}, global]),
                          {Original2, Result};
                     ({result, ReplaceString}, {Original, Result}) ->
                          Result2 = re:replace(Result,
                                               ReplaceString, "",
                                               [{return, list}, global]),
                          {Original, Result2};
                     (nodiff, {Original, Result}) ->
                          {Original, Result}
                  end, {OriginalXMLString3, XMLString2}, Diffs),
            ?assertEqual(OriginalFinalXML, ResultFinalXML);
        false ->
            ok
    end,
    {XML2, _Rest} = xmerl_scan:string(XMLString),
    CapableSwitchRecord2 = of_config:decode(XML2),
    ?assertEqual(true, is_record(CapableSwitchRecord2, capable_switch)),
    ?assertEqual(reset_operations(CapableSwitchRecord1),
                 reset_operations(CapableSwitchRecord2)).

reset_operations(Switch) ->
    reset_resources(reset_controllers(Switch)).

reset_controllers(#capable_switch{logical_switches = undefined} = CS) ->
    CS;
reset_controllers(#capable_switch{logical_switches = LS} = CS) ->
    CS#capable_switch{logical_switches = [reset_controllers(L) || L <- LS]};
reset_controllers(#logical_switch{controllers = undefined} = L) ->
    L;
reset_controllers(#logical_switch{controllers = CS} = L) ->
    L#logical_switch{controllers = [reset_controllers(C) || C <- CS]};
reset_controllers(#controller{} = C) ->
    C#controller{operation = undefined}.

reset_resources(#capable_switch{resources = undefined} = Switch) ->
    Switch;
reset_resources(#capable_switch{resources = Resources} = Switch) ->
    Switch#capable_switch{resources = [reset_resources(R) || R <- Resources]};
reset_resources(undefined) ->
    undefined;
reset_resources(#port{configuration = C, features = F} = P) ->
    C2 = case C of
             undefiend ->
                 undefiend;
             #port_configuration{} = PC ->
                 PC#port_configuration{operation = undefined}
         end,
    F2 = case F of
             undefined ->
                 undefined;
             #port_features{advertised = A} = PF ->
                 A2 = case A of
                          undefined ->
                              undefined;
                          #features{} = Feats ->
                              Feats#features{operation = undefined}
                      end,
                 #port_features{advertised = A2}
         end,
    P#port{operation = undefined,
           configuration = C2,
           features = F2};
reset_resources(#queue{} = Q) ->
    Q#queue{operation = undefined};
reset_resources(#certificate{} = C) ->
    C#certificate{operation = undefined};
reset_resources(#flow_table{} = F) ->
    F.

%% Fixtures --------------------------------------------------------------------

setup_11() ->
    application:load(of_config),
    application:set_env(of_config, of_config_schema, "of-config-1.1.xsd"),
    application:set_env(of_config, version, '1.1'),
    setup().

setup_111() ->
    application:load(of_config),
    application:set_env(of_config, of_config_schema, "of-config-1.1.1.xsd"),
    application:set_env(of_config, version, '1.1.1'),
    setup().

setup() ->
    error_logger:tty(true),
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
