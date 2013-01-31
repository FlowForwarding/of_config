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
-module(of_config_fixtures).

-export([get_config/0]).

-include("of_config.hrl").

get_config() ->
    #capable_switch{id = "CapableSwitch0",
                    resources = [#port{resource_id = "LogicalSwitch0-Port1",
                                       number = undefined,name = undefined,
                                       current_rate = undefined,max_rate = undefined,
                                       configuration = #port_configuration{admin_state = up,
                                                                           no_receive = false,no_forward = false,no_packet_in = false},
                                       state = undefined,
                                       features = #port_features{current = undefined,
                                                                 advertised = #features{rate = '1Gb-FD',
                                                                                        auto_negotiate = enabled,medium = fiber,pause = unsupported},
                                                                 supported = undefined,advertised_peer = undefined},
                                       tunnel = undefined},
                                 #port{resource_id = "LogicalSwitch0-Port2",
                                       number = undefined,name = undefined,
                                       current_rate = undefined,max_rate = undefined,
                                       configuration = #port_configuration{admin_state = up,
                                                                           no_receive = false,no_forward = false,no_packet_in = false},
                                       state = undefined,
                                       features = #port_features{current = undefined,
                                                                 advertised = #features{rate = 'Other',auto_negotiate = disabled,
                                                                                        medium = copper,pause = unsupported},
                                                                 supported = undefined,advertised_peer = undefined},
                                       tunnel = undefined},
                                 #port{resource_id = "LogicalSwitch1-Port1",
                                       number = undefined,name = undefined,
                                       current_rate = undefined,max_rate = undefined,
                                       configuration = #port_configuration{admin_state = up,
                                                                           no_receive = false,no_forward = false,no_packet_in = false},
                                       state = undefined,
                                       features = #port_features{current = undefined,
                                                                 advertised = #features{rate = 'Other',auto_negotiate = disabled,
                                                                                        medium = copper,pause = unsupported},
                                                                 supported = undefined,advertised_peer = undefined},
                                       tunnel = undefined},
                                 #port{resource_id = "LogicalSwitch1-Port2",
                                       number = undefined,name = undefined,
                                       current_rate = undefined,max_rate = undefined,
                                       configuration = #port_configuration{admin_state = up,
                                                                           no_receive = false,no_forward = false,no_packet_in = false},
                                       state = undefined,
                                       features = #port_features{current = undefined,
                                                                 advertised = #features{rate = 'Other',auto_negotiate = disabled,
                                                                                        medium = copper,pause = unsupported},
                                                                 supported = undefined,advertised_peer = undefined},
                                       tunnel = undefined}],
                    logical_switches = [#logical_switch{id = "LogicalSwitch0",
                                                        capabilities = undefined,datapath_id = "Datapath0",
                                                        enabled = true,check_controller_certificate = false,
                                                        lost_connection_behavior = failStandaloneMode,
                                                        controllers = [#controller{id = "Switch0Controller0",
                                                                                   role = equal,ip_address = "127.0.0.1/24",port = 6633,
                                                                                   local_ip_address = undefined,local_port = undefined,
                                                                                   protocol = tcp,state = undefined}],
                                                        resources = undefined},
                                        #logical_switch{id = "LogicalSwitch1",
                                                        capabilities = undefined,datapath_id = "Datapath1",
                                                        enabled = true,check_controller_certificate = false,
                                                        lost_connection_behavior = failStandaloneMode,
                                                        controllers = [#controller{id = "Switch1Controller0",
                                                                                   role = equal,ip_address = "127.0.0.1/24",port = 6634,
                                                                                   local_ip_address = undefined,local_port = undefined,
                                                                                   protocol = tcp,state = undefined}],
                                                        resources = undefined}]}.
