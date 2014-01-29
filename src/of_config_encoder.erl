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

when_version(Versions) ->
    Version = of_config:get_version(),
    {Version, Value} = lists:keyfind(Version, 1, Versions),
    Value.

-spec root_attributes('1.1' | '1.1.1') -> list(tuple(atom(), string())).
root_attributes(Version) ->
    case Version of
        '1.1' ->
            [{'xmlns', "urn:onf:params:xml:ns:onf:of12:config"}];
        '1.1.1' ->
            [{'xmlns', "urn:onf:of111:config:yang"}]
    end.

% OF-Config 1.1.1 XML schema requires the following order:
%    port
%    queue
%    owned-certitificate
%    external-certitificate
%    flow-table
sort_resources(undefined) ->
    undefined;
sort_resources(Resources) when is_list(Resources) ->
    {[], Sorted} = lists:foldl(fun(T, {Rest, Acc}) ->
        {L, Rest2} = lists:partition(fun(E) ->
            ET = element(1, E),
            case T of
                {certificate, SubType} ->
                    ET =:= certificate andalso E#certificate.type =:= SubType;
                Type ->
                    ET =:= Type
            end
        end, Rest),
        {Rest2, Acc ++ L}
    end, {Resources, []}, [
        port,
        queue,
        {certificate, owned},
        {certificate, external},
        flow_table]),
    Sorted.

-spec simple_form(#capable_switch{}) -> simple_form().
simple_form(#capable_switch{id = Id,
                            configuration_points = ConfigurationPoints,
                            resources = Resources,
                            logical_switches = LogicalSwitches}) ->
    SortedResources = sort_resources(Resources),
    L = [element(id, string, Id),
         element('configuration-points', nested_list, ConfigurationPoints),
         element('resources', nested_list, SortedResources),
         element('logical-switches', nested_list, LogicalSwitches)],
    {'capable-switch', root_attributes(of_config:get_version()), only_valid_elements(L)};
simple_form(#configuration_point{id = Id,
                                 uri = Uri,
                                 protocol = Protocol}) ->
    element('configuration-point', list,
            [element(id, string, Id),
             element(uri, string, Uri),
             element(protocol, atom, Protocol)]);
simple_form(#features{rate = Rate,
                      auto_negotiate = AutoNegotiate,
                      medium = Medium,
                      pause = Pause}) ->
    [element('rate', atom, Rate),
     element('auto-negotiate', atom, AutoNegotiate),
     element('medium', atom, Medium),
     element('pause', atom, Pause)];
simple_form(#port_features{current = Current,
                           advertised = Advertised,
                           supported = Supported,
                           advertised_peer = Peer}) ->
    [element('current', nested, Current),
     element('advertised', nested, Advertised),
     element('supported', nested, Supported),
     element('advertised-peer', nested, Peer)];
simple_form(#port_configuration{admin_state = AdminState,
                                no_receive = NoReceive,
                                no_forward = NoForward,
                                no_packet_in = NoPacketIn}) ->
    [element('admin-state', atom, AdminState),
     element('no-receive', atom, NoReceive),
     element('no-forward', atom, NoForward),
     element('no-packet-in', atom, NoPacketIn)];
simple_form(#port_state{oper_state = OperState,
                        blocked = Blocked,
                        live = Live}) ->
    [element('oper-state', atom, OperState),
     element('blocked', atom, Blocked),
     element('live', atom, Live)];
simple_form(#port{resource_id = ResourceId,
                  number = Number,
                  name = Name,
                  current_rate = CurrentRate,
                  max_rate = MaxRate,
                  configuration = Configuration,
                  state = State,
                  features = Features,
                  tunnel = Tunnel}) ->
    element('port', list,
            [element('resource-id', string, ResourceId),
             element('number', integer, Number),
             element('name', string, Name),
             element('current-rate', integer, CurrentRate),
             element('max-rate', integer, MaxRate),
             element('configuration', nested, Configuration),
             element('state', nested, State),
             element('features', nested, Features),
             element('tunnel', integer, Tunnel)]);
simple_form(#queue_properties{min_rate = MinRate,
                              max_rate = MaxRate,
                              experimenters = Experimenters
                             }) ->
    [element('min-rate', integer, MinRate),
     element('max-rate', integer, MaxRate)]
        ++ case Experimenters of
               undefined ->
                   [];
               _ ->
                   lists:map(fun(E) ->
                                     element('experimenter', integer, E)
                             end, Experimenters)
           end;
simple_form(#queue{resource_id = ResourceId,
                   id = Id,
                   port = Port,
                   properties = Properties}) ->
    element('queue', list,
            [element('resource-id', string, ResourceId),
             element('id', integer, Id),
             element('port', integer, Port),
             element('properties', nested, Properties)]);
%% TODO: Add DSAKeyValue
simple_form(#private_key_rsa{modulus = Modulus,
                             exponent = Exponent}) ->
    element('RSAKeyValue', list,
            [element('Modulus', string, Modulus),
             element('Exponent', string, Exponent)]);
simple_form(#certificate{resource_id = ResourceId,
                         type = owned,
                         certificate = Certificate,
                         private_key = PrivateKey}) ->
    element('owned-certificate', list,
            [element('resource-id', string, ResourceId),
             element('certificate', string, Certificate),
             element('private-key', nested, PrivateKey)]);
simple_form(#certificate{resource_id = ResourceId,
                         type = external,
                         certificate = Certificate}) ->
    element('external-certificate', list,
            [element('resource-id', string, ResourceId),
             element('certificate', string, Certificate)]);
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
    element('flow-table', list,
            [element('resource-id', string, RId),
             element('max-entries', integer, MaxEntries),
             element('next-tables', {'table-id', integer}, Tables),
             element('instructions', {'type', atom}, Instructions),
             element('matches', {'type', atom}, Matches),
             element('write-actions', {'type', atom}, WriteActions),
             element('apply-actions', {'type', atom}, ApplyActions),
             element('write-setfields', {'type', atom}, WriteSetfields),
             element('apply-setfields', {'type', atom}, ApplySetfields),
             element('wildcards', {'type', atom}, Wildcards),
             element('metadata-match', string, to_hex(MetadataMatch)),
             element('metadata-write', string, to_hex(MetadataWrite))]);
simple_form(#capabilities{max_buffered_packets = MBP,
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
                          instruction_types = IT}) ->
    [element('max-buffered-packets', integer, MBP),
     element('max-tables', integer, MT),
     element('max-ports', integer, MP),
     element('flow-statistics', atom, FS),
     element('table-statistics', atom, TS),
     element('port-statistics', atom, PS),
     element('group-statistics', atom, GS),
     element('queue-statistics', atom, QS),
     element('reassemble-ip-fragments', atom, RIF),
     element('block-looping-ports', atom, BLP),
     element('reserved-port-types', {'type', atom}, RPT),
     element('group-types', {'type', atom}, GT),
     element('group-capabilities', {'capability', atom}, GC),
     element('action-types', {'type', atom}, AT),
     element('instruction-types', {'type', atom}, IT)];
simple_form(#logical_switch{id = Id,
                            capabilities = Capabilities,
                            datapath_id = DId,
                            enabled = E,
                            check_controller_certificate = CCC,
                            lost_connection_behavior = LCB,
                            controllers = Controllers,
                            resources = Resources
                           }) ->
    Switch = when_version([{'1.1', 'logical-switch'},
                           {'1.1.1', 'switch'}]),
    element(Switch, list,
            [element('id', string, Id),
             element('capabilities', nested, Capabilities),
             element('datapath-id', string, DId),
             element('enabled', atom, E),
             element('check-controller-certificate', atom, CCC),
             element('lost-connection-behavior', atom, LCB),
             element('controllers', nested_list, Controllers),
             case Resources of
                 undefined ->
                     undefined;
                 _ ->
                     {'resources', lists:map(fun({port, Value}) ->
                                                     {'port', [Value]};
                                                ({queue, Value}) ->
                                                     {'queue', [Value]};
                                                ({certificate, Value}) ->
                                                     {'certificate', [Value]};
                                                ({flow_table, Value}) ->
                                                     {'flow-table', [Value]}
                                             end, Resources)}
             end
            ]);
simple_form(#controller_state{connection_state = State,
                              current_version = Version,
                              supported_versions = Supported
                             }) ->
    SupportedVersions =
        case of_config:get_version() of
            '1.1' ->
                [element('supported-versions', {'version', atom}, Supported)];
            '1.1.1' ->
                [element('supported-versions', atom, V) || V <- Supported]
        end,
    [element('connection-state', atom, State),
     element('current-version', atom, Version)]
        ++ SupportedVersions;
simple_form(#controller{id = Id,
                        role = Role,
                        ip_address = IP,
                        port = Port,
                        local_ip_address = LocalIP,
                        local_port = LocalPort,
                        protocol = Proto,
                        state = State
                       }) ->
    element('controller', list,
            [element('id', string, Id),
             element('role', atom, Role),
             element('ip-address', string, IP),
             element('port', integer, Port),
             element('local-ip-address', string, LocalIP),
             element('local-port', integer, LocalPort),
             element('protocol', atom, Proto),
             element('state', nested, State)
            ]).

only_valid_elements(Elements) when is_list(Elements) ->
    lists:filter(fun(undefined) ->
                         false;
                    (_) ->
                         true
                 end, Elements);
only_valid_elements(Element) ->
    only_valid_elements([Element]).

element(_Name, _Type, undefined) ->
    undefined;
element(Name, string, Value) ->
    {Name, [Value]};
element(Name, atom, Value) ->
    {Name, [atom_to_list(Value)]};
element(Name, integer, Value) ->
    {Name, [integer_to_list(Value)]};
element(Name, nested, Value) ->
    {Name, only_valid_elements(simple_form(Value))};
element(Name, list, Value) ->
    {Name, only_valid_elements(Value)};
element(Name, nested_list, Elements) ->
    {Name, only_valid_elements(lists:map(fun(E) ->
                                                 simple_form(E)
                                         end, Elements))};
element(Name, {NestedName, Type}, Elements) ->
    {Name, only_valid_elements(lists:map(fun(E) ->
                                                 element(NestedName, Type, E)
                                         end, Elements))}.

to_hex(Binary) ->
    to_hex(Binary, []).

to_hex(<<>>, Hex) ->
    lists:flatten(lists:reverse(Hex));
to_hex(<<B1:4, B2:4, Binary/binary>>, Hex) ->
    I1 = integer_to_list(B1, 16),
    I2 = integer_to_list(B2, 16),
    to_hex(Binary, [I2, I1 | Hex]).
