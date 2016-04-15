%%%----------------------------------------------------------------------

%%% File    : mod_muc_offline_post.erl
%%% Author  : Adam Duke <adam.v.duke@gmail.com>
%%% Purpose : Forward offline messages to an arbitrary url
%%% Created : 12 Feb 2012 by Adam Duke <adam.v.duke@gmail.com>
%%%
%%%
%%% Copyright (C) 2012   Adam Duke
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_offline_post).
-author('rgeorge@midnightweb.net').

-behaviour(gen_mod).

-export([start/2,
  stop/1]).

-export([on_filter_packet/1]).


start(Host, _Opts) ->
  ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 0).

stop(_Host) ->
  %?DEBUG("Bye bye, ejabberd world!", []),
  ok.

on_filter_packet({From, To, XML} = Packet) ->
  %% does something with a packet
  %% should return modified Packet or atom `drop` to drop the packet
  ?INFO_MSG("filtering packet :D", []),

  Packet_Type = xml:get_tag_attr_s("type", XML),

  case Packet_Type of
    "message" ->
      ?INFO_MSG("Its a message...", []);
    _Other ->
      ?INFO_MSG("Other kind of presence~n~p", [Packet])
  end,

  %xml:get_tag_attr_s(list_to_binary("type"),Packet),

  % case Packet_Type of
  %   "message" ->
  %       process_received_message(Packet);
  %   _ ->
  %       Packet
  % end.

  Packet.
