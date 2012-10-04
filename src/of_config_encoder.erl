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
%% @doc Module for transformation of #capable_switch{} internal record to
%% Xmerl simple format (list of nested tuples).
-module(of_config_encoder).

%% API
-export([to_simple_form/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("of_config.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-type simple_form() :: {Tag :: atom(),
                        Attributes :: [{Key :: atom(), Value :: string()}],
                        Content :: [simple_form()]}
                     | {Tag :: atom(), Content :: [simple_form()]}
                     | {Tag :: atom()}.

-spec to_simple_form(#capable_switch{}) -> [simple_form()].
to_simple_form(Config) ->
    simple_form(Config).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

-spec root_attributes() -> list(tuple(atom(), string())).
root_attributes() ->
    [{'xmlns', "urn:onf:params:xml:ns:onf:of12:config"},
     {'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"},
     {'xmlns:ds', "http://www.w3.org/2000/09/xmldsig#"},
     {'xsi:schemaLocation', "urn:onf:params:xml:ns:onf:of12:config of-config-1.1.xsd"}].

-spec simple_form(#capable_switch{}) -> simple_form().
simple_form(#capable_switch{id = Id,
                            configuration_points = CP,
                            resources = R,
                            logical_switches = LS}) ->
    {'capable-switch', root_attributes(), [element(id, string, Id),
                                           list_to_simple_form('configuration-points', CP),
                                           list_to_simple_form('resources', R),
                                           list_to_simple_form('logical-switches', LS)]};
simple_form(#configuration_point{id = Id,
                                 uri = Uri,
                                 protocol = Proto}) ->
    {'configuration-point', [element(id, string, Id),
                             element(uri, string, Uri),
                             element(protocol, atom, Proto)]};
simple_form(#port{resource_id = RId,
                  number = Number,
                  name = Name,
                  current_rate = CR,
                  max_rate = MR,
                  configuration = #port_configuration{admin_state = AS,
                                                      no_receive = NR,
                                                      no_forward = NF,
                                                      no_packet_in = NPI},
                  state = #port_state{oper_state = OS,
                                      blocked = B,
                                      live = L},
                  features = #port_features{current = #features{
                                              rate = RateCurr,
                                              auto_negotiate = ANCurr,
                                              medium = MCurr,
                                              pause = PCurr},
                                            advertised = #features{
                                              rate = RateAdv,
                                              auto_negotiate = ANAdv,
                                              medium = MAdv,
                                              pause = PAdv},
                                            supported = #features{
                                              rate = RateSup,
                                              auto_negotiate = ANSup,
                                              medium = MSup,
                                              pause = PSup},
                                            advertised_peer = #features{
                                              rate = RateAP,
                                              auto_negotiate = ANAP,
                                              medium = MAP,
                                              pause = PAP}},
                  tunnel = Tunnel}) ->
    {'port', [element('resource-id', string, RId),
              element('number', integer, Number),
              element('name', string, Name),
              element('current-rate', integer, CR),
              element('max-rate', integer, MR),
              nested_elements('configuration', [{'admin-state', atom, AS},
                                                {'no-receive', atom, NR},
                                                {'no-forward', atom, NF},
                                                {'no-packet-in', atom, NPI}
                                               ]),
              nested_elements('state', [{'oper-state', atom, OS},
                                        {'blocked', atom, B},
                                        {'live', atom, L}]),
              {features, [nested_elements('current', [{'rate', atom, RateCurr},
                                                      {'auto-negotiate', atom, ANCurr},
                                                      {'medium', atom, MCurr},
                                                      {'pause', atom, PCurr}]),
                          nested_elements('advertised', [{'rate', atom, RateAdv},
                                                         {'auto-negotiate', atom, ANAdv},
                                                         {'medium', atom, MAdv},
                                                         {'pause', atom, PAdv}]),
                          nested_elements('supported', [{'rate', atom, RateSup},
                                                        {'auto-negotiate', atom, ANSup},
                                                        {'medium', atom, MSup},
                                                        {'pause', atom, PSup}]),
                          nested_elements('advertised-peer', [{'rate', atom, RateAP},
                                                              {'auto-negotiate', atom, ANAP},
                                                              {'medium', atom, MAP},
                                                              {'pause', atom, PAP}])
                         ]}] ++ case Tunnel of
                                    undefined ->
                                        [];
                                    Tunnel ->
                                        {tunnel, []}
                                end};
simple_form(#queue{resource_id = RId,
                   id = Id,
                   port = Port,
                   properties = #queue_properties{min_rate = MinRate,
                                                  max_rate = MaxRate,
                                                  experimenters = Exps
                                                 }}) ->
    {'queue', [element('resource-id', string, RId),
               element(id, integer, Id),
               element(port, integer, Port),
               {'properties', lists:append([[element('min-rate', integer, MinRate),
                                             element('max-rate', integer, MaxRate)],
                                            lists:map(fun(E) ->
                                                              element('experimenter', integer, E)
                                                      end, Exps)])}]};
simple_form(#certificate{resource_id = RId,
                         type = owned,
                         certificate = Cert,
                         private_key = #private_key_rsa{modulus = Modulus,
                                                        exponent = Exponent
                                                       }
                        }) ->
    {'owned-certificate', [element('resource-id', string, RId),
                           element('certificate', string, Cert),
                           %% TODO: Add DSAKeyValue
                           {'private-key', [{'RSAKeyValue', [element('Modulus', string, Modulus),
                                                             element('Exponent', string, Exponent)]}]}
                          ]};
simple_form(#certificate{resource_id = RId,
                         type = external,
                         certificate = Cert
                        }) ->
    {'external-certificate', [element('resource-id', string, RId),
                              element('certificate', string, Cert)]};
simple_form(#flow_table{resource_id = RId,
                        max_entries = MaxEntries,
                        next_tables = Tables,
                        instructions = Instructions,
                        matches = Matches,
                        write_actions = WriteActions,
                        apply_actions = ApplyActions,
                        write_setfields = WriteSetfields,
                        apply_setfields = ApplySetfields,
                        wildcards = Wildcards,
                        metadata_match = MetadataMatch,
                        metadata_write = MetadataWrite
                       }) ->
    {'flow-table', [element('resource-id', string, RId),
                    element('max-entries', integer, MaxEntries),
                    nested_list('next-tables', 'table-id', integer, Tables),
                    nested_list('instructions', 'type', atom, Instructions),
                    nested_list('matches', 'type', atom, Matches),
                    nested_list('write-actions', 'type', atom, WriteActions),
                    nested_list('apply-actions', 'type', atom, ApplyActions),
                    nested_list('write-setfields', 'type', atom, WriteSetfields),
                    nested_list('apply-setfields', 'type', atom, ApplySetfields),
                    nested_list('wildcards', 'type', atom, Wildcards),
                    element('metadata-match', string, MetadataMatch),
                    element('metadata-write', string, MetadataWrite)
                   ]};
simple_form(#logical_switch{id = Id,
                            capabilities = #capabilities{max_buffered_packets = MBP,
                                                         max_tables = MT,
                                                         max_ports = MP,
                                                         flow_statistics = FS,
                                                         table_statistics = TS,
                                                         port_statistics = PS,
                                                         group_statistics = GS,
                                                         queue_statistics = QS,
                                                         reassemble_ip_fragments = RIF,
                                                         block_looping_ports = BLP,
                                                         reserved_port_types = RPT,
                                                         group_types = GT,
                                                         group_capabilities = GC,
                                                         action_types = AT,
                                                         instruction_types = IT
                                                        },
                            datapath_id = DId,
                            enabled = E,
                            check_controller_certificate = CCC,
                            lost_connection_behavior = LCB,
                            controllers = Controllers,
                            resources = Resources
                           }) ->
    Switch = case application:get_env(of_config, version) of
                 {ok, '1.1'} ->
                     'logical-switch';
                 {ok, '1.1.1'} ->
                     'switch'
             end,
    {Switch, [element('id', string, Id),
              {'capabilities', [element('max-buffered-packets', integer, MBP),
                                element('max-tables', integer, MT),
                                element('max-ports', integer, MP),
                                element('flow-statistics', atom, FS),
                                element('table-statistics', atom, TS),
                                element('port-statistics', atom, PS),
                                element('group-statistics', atom, GS),
                                element('queue-statistics', atom, QS),
                                element('reassemble-ip-fragments', atom, RIF),
                                element('block-looping-ports', atom, BLP),
                                nested_list('reserved-port-types', 'type', atom, RPT),
                                nested_list('group-types', 'type', atom, GT),
                                nested_list('group-capabilities', 'capability', atom, GC),
                                nested_list('action-types', 'type', atom, AT),
                                nested_list('instruction-types', 'type', atom, IT)
                               ]},
              element('datapath-id', string, DId),
              element('enabled', atom, E),
              element('check-controller-certificate', atom, CCC),
              element('lost-connection-behavior', atom, LCB),
              list_to_simple_form('controllers', Controllers),
              {'resources', lists:map(fun({port, Value}) ->
                                              {'port', [Value]};
                                         ({queue, Value}) ->
                                              {'queue', [Value]};
                                         ({certificate, Value}) ->
                                              {'certificate', [Value]};
                                         ({flow_table, Value}) ->
                                              {'flow-table', [Value]}
                                      end, Resources)}
             ]};
simple_form(#controller{id = Id,
                        role = Role,
                        ip_address = IP,
                        port = Port,
                        local_ip_address = LocalIP,
                        local_port = LocalPort,
                        protocol = Proto,
                        state = #controller_state{connection_state = State,
                                                  current_version = Version,
                                                  supported_versions = Supported
                                                 }
                       }) ->
    {'controller', [element('id', string, Id),
                    element('role', atom, Role),
                    element('ip-address', string, IP),
                    element('port', integer, Port),
                    element('local-ip-address', string, LocalIP),
                    element('local-port', integer, LocalPort),
                    element('protocol', atom, Proto),
                    {'state', [element('connection-state', atom, State),
                               element('current-version', atom, Version),
                               nested_list('supported-versions', 'version', atom, Supported)
                              ]}
                   ]}.

element(Name, string, Value) ->
    {Name, [Value]};
element(Name, atom, Value) ->
    {Name, [atom_to_list(Value)]};
element(Name, integer, Value) ->
    {Name, [integer_to_list(Value)]}.

nested_list(WrapperName, ElementName, ElementType, Elements) ->
    {WrapperName, lists:map(fun(E) ->
                                    element(ElementName, ElementType, E)
                            end,Elements)}.

nested_elements(WrapperName, Elements) ->
    {WrapperName, [element(Name, Type, Value) || {Name, Type, Value} <- Elements]}.

list_to_simple_form(_WrapperName, undefined) ->
    undefined;
list_to_simple_form(WrapperName, List) ->
    {WrapperName, [simple_form(E) || E <- List]}.
