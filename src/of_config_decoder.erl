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
%% @doc Module for transformation of Xmerl tuple representation to the
%% #capable_switch{} record.
-module(of_config_decoder).

-export([to_capable_switch/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("of_config.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec to_capable_switch(xmerl_scan:xmlElement()) -> #capable_switch{}.
to_capable_switch(XML) ->
    transform_xml(XML).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% <capable-switch/>
transform_xml(#xmlElement{name = 'capable-switch', content = C}) ->
    #capable_switch{id = get_from_child(string, id, C),
                    configuration_points = transform_xml(get_child('configuration-points', C)),
                    resources = transform_xml(get_child(resources, C)),
                    logical_switches = transform_xml(get_child('logical-switches', C))};
%% <configuration-points/>
transform_xml(#xmlElement{name = 'configuration-points', content = C}) ->
    transform_all_children(C);
transform_xml(#xmlElement{name = 'configuration-point', content = C}) ->
    #configuration_point{id = get_from_child(string, id, C),
                         uri = get_from_child(string, uri, C),
                         protocol = get_from_child(atom, protocol, C)};
%% <resources/>
transform_xml(#xmlElement{name = 'resources', content = C}) ->
    transform_all_children(C);
transform_xml(#xmlElement{name = 'port', content = C, attributes = Attrs}) ->
    #port{operation = get_operation(Attrs),
          resource_id = get_from_child(string, 'resource-id', C),
          number = get_from_child(integer, number, C),
          name = get_from_child(string, name, C),
          current_rate = get_from_child(integer, 'current-rate', C),
          max_rate = get_from_child(integer, 'max-rate', C),
          configuration = transform_xml(get_child(configuration, C)),
          state = transform_xml(get_child(state, C)),
          features = transform_xml(get_child(features, C)),
          tunnel = transform_xml(get_child(tunnel, C))
         };
transform_xml(#xmlElement{name = 'configuration', content = C}) ->
    #port_configuration{admin_state = get_from_child(atom, 'admin-state', C),
                        no_receive = get_from_child(atom, 'no-receive', C),
                        no_forward = get_from_child(atom, 'no-forward', C),
                        no_packet_in = get_from_child(atom, 'no-packet-in', C)};
transform_xml(#xmlElement{name = 'state', content = C}) ->
    #port_state{oper_state = get_from_child(atom, 'oper-state', C),
                blocked = get_from_child(atom, blocked, C),
                live = get_from_child(atom, live, C)};
transform_xml(#xmlElement{name = 'features', content = C}) ->
    #port_features{current = transform_xml(get_child(current, C)),
                   advertised = transform_xml(get_child(advertised, C)),
                   supported = transform_xml(get_child(supported, C)),
                   advertised_peer = transform_xml(get_child('advertised-peer', C))};
transform_xml(#xmlElement{name = 'current', content =C}) ->
    #features{rate = get_from_child(atom, 'rate', C),
              auto_negotiate = get_from_child(atom, 'auto-negotiate', C),
              medium = get_from_child(atom, 'medium', C),
              pause = get_from_child(atom, 'pause', C)
             };
transform_xml(#xmlElement{name = 'advertised', content =C}) ->
    #features{rate = get_from_child(atom, 'rate', C),
              auto_negotiate = get_from_child(atom, 'auto-negotiate', C),
              medium = get_from_child(atom, 'medium', C),
              pause = get_from_child(atom, 'pause', C)
             };
transform_xml(#xmlElement{name = 'supported', content =C}) ->
    #features{rate = get_from_child(atom, 'rate', C),
              auto_negotiate = get_from_child(atom, 'auto-negotiate', C),
              medium = get_from_child(atom, 'medium', C),
              pause = get_from_child(atom, 'pause', C)
             };
transform_xml(#xmlElement{name = 'advertised-peer', content =C}) ->
    #features{rate = get_from_child(atom, 'rate', C),
              auto_negotiate = get_from_child(atom, 'auto-negotiate', C),
              medium = get_from_child(atom, 'medium', C),
              pause = get_from_child(atom, 'pause', C)
             };
%% TODO all tunnels
transform_xml(#xmlElement{name = 'ipgre-tunnel', content = _C}) ->
    #ip_in_gre_tunnel{};
transform_xml(#xmlElement{name = 'vxlan-tunnel', content = _C}) ->
    #vxlan_tunnel{};
transform_xml(#xmlElement{name = 'nvgre-tunnel', content = _C}) ->
    #nvgre_tunnel{};
transform_xml(#xmlElement{name = 'queue', content = C, attributes = Attrs}) ->
    #queue{operation = get_operation(Attrs),
           resource_id = get_from_child(string, 'resource-id', C),
           id = get_from_child(integer, 'id', C),
           port = get_from_child(integer, 'port', C),
           properties = transform_xml(get_child(properties, C))
          };
transform_xml(#xmlElement{name = 'properties', content = C}) ->
    #queue_properties{min_rate = get_from_child(integer, 'min-rate', C),
                      max_rate = get_from_child(integer, 'max-rate', C),
                      experimenters = lists:foldr(fun(#xmlElement{name = 'experimenter'} = E, Acc) ->
                                                          [get_value(integer, E) | Acc];
                                                     (_, Acc) ->
                                                          Acc
                                                  end, [], C)};
transform_xml(#xmlElement{name = 'owned-certificate', content = C, attributes = Attrs}) ->
    #certificate{operation = get_operation(Attrs),
                 resource_id = get_from_child(string, 'resource-id', C),
                 type = owned,
                 certificate = get_from_child(string, certificate, C),
                 private_key = transform_xml(get_child('private-key', C))               
                };
transform_xml(#xmlElement{name = 'private-key', content = C}) ->
    case get_child('RSAKeyValue', C) of
        undefined ->
            case get_child('DSAKeyValue', C) of
                undefined ->
                    undefined;
                #xmlElement{name = 'DSAKeyValue', content = DSAChildren} ->
                    #private_key_dsa{p = get_from_child(string, 'p', DSAChildren),
                                     q = get_from_child(string, 'q', DSAChildren),
                                     g = get_from_child(string, 'g', DSAChildren),
                                     y = get_from_child(string, 'y', DSAChildren),
                                     j = get_from_child(string, 'j', DSAChildren),
                                     seed = get_from_child(string, 'seed', DSAChildren),
                                     pgen_counter = get_from_child(string, 'pgen_counter', DSAChildren)}
            end;
        #xmlElement{name = 'RSAKeyValue', content = RSAChildren} ->
            #private_key_rsa{modulus = get_from_child(string, 'Modulus', RSAChildren),
                             exponent = get_from_child(string, 'Exponent', RSAChildren)}
    end;
transform_xml(#xmlElement{name = 'external-certificate', content = C, attributes = Attrs}) ->
    #certificate{operation = get_operation(Attrs),
                 resource_id = get_from_child(string, 'resource-id', C),
                 type = external,
                 certificate = get_from_child(string, certificate, C)
                };
transform_xml(#xmlElement{name = 'flow-table', content = C}) ->
    #flow_table{resource_id = get_from_child(string, 'resource-id', C),
                max_entries = get_from_child(integer, 'max-entries', C),
                next_tables = transform_xml(get_child('next-tables', C)),
                instructions = transform_xml(get_child('instructions', C)),
                matches = transform_xml(get_child('matches', C)),
                write_actions = transform_xml(get_child('write-actions', C)),
                apply_actions = transform_xml(get_child('apply-actions', C)),
                write_setfields = transform_xml(get_child('write-setfields', C)),
                apply_setfields = transform_xml(get_child('apply-setfields', C)),
                wildcards = transform_xml(get_child('wildcards', C)),
                metadata_match = hex_to_bin(get_from_child(string,
                                                           'metadata-match', C)),
                metadata_write = hex_to_bin(get_from_child(string,
                                                           'metadata-write', C))};
transform_xml(#xmlElement{name = 'next-tables', content = C}) ->
    get_all_children(integer, C);
transform_xml(#xmlElement{name = 'instructions', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'matches', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'write-actions', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'apply-actions', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'write-setfields', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'apply-setfields', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'wildcards', content = C}) ->
    get_all_children(atom, C);
%% <logical-switches/>
transform_xml(#xmlElement{name = 'logical-switches', content = C}) ->
    transform_all_children(C);
transform_xml(#xmlElement{name = 'switch'} = E) ->
    %% Support for difference in OF-Config 1.1 and 1.1.1 where 'logical-switch'
    %% was renamed to 'switch' element
    transform_xml(E#xmlElement{name = 'logical-switch'});
transform_xml(#xmlElement{name = 'logical-switch', content = C}) ->
    #logical_switch{id = get_from_child(string, id, C),
                    capabilities = transform_xml(get_child(capabilities, C)),
                    datapath_id = get_from_child(string, 'datapath-id', C),
                    enabled = get_from_child(atom, enabled, C),
                    check_controller_certificate = get_from_child(atom, 'check-controller-certificate', C),
                    lost_connection_behavior = get_from_child(atom, 'lost-connection-behavior', C),
                    controllers = transform_xml(get_child(controllers, C)),
                    resources = transform_xml(get_child_with_name(resources, 'logical-switch-resources', C))
                   };
transform_xml(#xmlElement{name = 'capabilities', content = C}) ->
    #capabilities{max_buffered_packets = get_from_child(integer, 'max-buffered-packets', C),
                  max_tables = get_from_child(integer, 'max-tables', C),
                  max_ports = get_from_child(integer, 'max-ports', C),
                  flow_statistics = get_from_child(atom, 'flow-statistics', C),
                  table_statistics = get_from_child(atom, 'table-statistics', C),
                  port_statistics = get_from_child(atom, 'port-statistics', C),
                  group_statistics = get_from_child(atom, 'group-statistics', C),
                  queue_statistics = get_from_child(atom, 'queue-statistics', C),
                  reassemble_ip_fragments = get_from_child(atom, 'reassemble-ip-fragments', C),
                  block_looping_ports = get_from_child(atom, 'block-looping-ports', C),
                  reserved_port_types = transform_xml(get_child('reserved-port-types', C)),
                  group_types = transform_xml(get_child('group-types', C)),
                  group_capabilities = transform_xml(get_child('group-capabilities', C)),
                  action_types = transform_xml(get_child('action-types', C)),
                  instruction_types = transform_xml(get_child('instruction-types', C))
                 };
transform_xml(#xmlElement{name = 'reserved-port-types', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'group-types', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'group-capabilities', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'action-types', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'instruction-types', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'controllers', content = C}) ->
    transform_all_children(C);
transform_xml(#xmlElement{name = 'controller', content = C, attributes = Attrs}) ->
    #controller{operation = get_operation(Attrs),
                id = get_from_child(string, id, C),
                role = get_from_child(atom, role, C),
                ip_address = get_from_child(string, 'ip-address', C),
                port = get_from_child(integer, 'port', C),
                local_ip_address = get_from_child(string, 'local-ip-address', C),
                local_port = get_from_child(integer, 'local-port', C),
                protocol = get_from_child(atom, 'protocol', C),
                state = transform_xml(get_child_with_name('state', 'controller-state', C))
               };
transform_xml(#xmlElement{name = 'controller-state', content = C}) ->
    #controller_state{connection_state = get_from_child(atom, 'connection-state', C),
                      current_version = get_from_child(atom, 'current-version', C),
                      supported_versions = transform_xml(get_child('supported-versions', C))
                     };
transform_xml(#xmlElement{name = 'supported-versions', content = C}) ->
    get_all_children(atom, C);
transform_xml(#xmlElement{name = 'logical-switch-resources', content = C}) ->
    lists:foldr(fun(#xmlElement{name = 'port'} = E, Acc) ->
                        [{port, get_value(string, E)} | Acc];
                   (#xmlElement{name = 'queue'} = E, Acc) ->
                        [{queue, get_value(string, E)} | Acc];
                   (#xmlElement{name = 'certificate'} = E, Acc)->
                        [{certificate, get_value(string, E)} | Acc];
                   (#xmlElement{name = 'flow-table'} = E, Acc) ->
                        [{flow_table, get_value(string, E)} | Acc];
                   (_, Acc) ->
                        Acc
              end, [], C);
transform_xml(_XML) ->
    undefined.

-spec transform_all_children(list(#xmlElement{})) -> list(any()).
transform_all_children(Children) ->
    lists:foldr(fun(C, Acc) ->
                        case transform_xml(C) of
                            undefined ->
                                Acc;
                            TransC ->
                                [TransC | Acc]
                        end
                end, [], Children).

-type payload_type() :: string
                      | atom
                      | integer.

-type payload() :: string()
                 | atom()
                 | integer().

-spec get_all_children(payload_type(), list(#xmlElement{})) -> list(any()).
get_all_children(Type, Children) ->
    lists:foldr(fun(#xmlElement{} = C, Acc) ->
                        [get_value(Type, C) | Acc];
                   (_, Acc) ->
                        Acc
                end, [], Children).

-spec get_from_child(payload_type(), atom(), list(#xmlElement{})) -> payload().
get_from_child(Type, ChildName, Children) ->
    Child = get_child(ChildName, Children),
    get_value(Type, Child).

-spec get_child(atom(), list(#xmlElement{})) -> #xmlElement{} | false.
get_child(Name, Children) ->
    lists:keyfind(Name, 2, Children).

-spec get_child_with_name(atom(), atom(), list(#xmlElement{})) -> #xmlElement{} | false.
get_child_with_name(Name, NewName, Children) ->
    case get_child(Name, Children) of
        false ->
            false;
        Child ->
            Child#xmlElement{name = NewName}
    end.

-spec get_value(payload_type(), #xmlElement{}) -> payload().
get_value(string, #xmlElement{content = [#xmlText{value = Value}],
                              attributes = [#xmlAttribute{name = operation,
                                                          value = Operation}]}) ->
    {list_to_atom(Operation), Value};
get_value(string, #xmlElement{content = [#xmlText{value = Value}]}) ->
    Value;
get_value(atom, #xmlElement{content = [#xmlText{value = Value}],
                            attributes = [#xmlAttribute{name = operation,
                                                        value = Operation}]}) ->
    {list_to_atom(Operation), list_to_atom(Value)};
get_value(atom, #xmlElement{content = [#xmlText{value = Value}]}) ->
    list_to_atom(Value);
get_value(integer, #xmlElement{content = [#xmlText{value = Value}],
                               attributes = [#xmlAttribute{name = operation,
                                                           value = Operation}]}) ->
    {list_to_atom(Operation), list_to_integer(Value)};
get_value(integer, #xmlElement{content = [#xmlText{value = Value}]}) ->
    list_to_integer(Value);
get_value(_, _) ->
    undefined.

get_operation(Attrs) ->
    case lists:keyfind(operation, #xmlAttribute.name, Attrs) of
        #xmlAttribute{value=Value} ->
            list_to_atom(Value);
        false ->
            undefined
    end.

hex_to_bin(String) ->
    Int = list_to_integer(String, 16),
    <<Int:64>>.
