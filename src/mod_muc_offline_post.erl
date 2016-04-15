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
  init/2,
  stop/1,
  grab_packet/4,
  grab_notice/3,
  send_notice/3,
  mod_opt_type/1]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

start(Host, Opts) ->
  Version = "0.3",
  ?INFO_MSG("Starting mod_muc_offline_post v.~s", [Version]),
  register(?PROCNAME, spawn(?MODULE, init, [Host, Opts])),
  ok.

init(Host, _Opts) ->
  inets:start(),
  ssl:start(),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, grab_packet, 10),
  ok.

stop(Host) ->
  ?INFO_MSG("Stopping mod_muc_offline_post", []),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, grab_packet, 10),
  ok.

grab_packet(Packet, _C2SState, From, To) ->
  ?INFO_MSG("Called grab_packet", []),
  grab_notice(Packet, From, To),
  Packet.

grab_notice(Packet = #xmlel{name = <<"message">>, attrs = Attrs}, From, To) ->
  ?INFO_MSG("Called grab_notice", []),
  case fxml:get_attr_s(<<"type">>, Attrs) of
    <<"groupchat">> -> %% mod_muc_log already does it
      send_notice(From, To, Packet),
      ok;
    _ -> ?DEBUG("dropping all: packet",[])
  end.


send_notice(From, To, Packet = #xmlel{name = <<"body">>, attrs = Attrs}) ->
  ?INFO_MSG("Called send_notice ~p~n", [Packet]),
  ?INFO_MSG("------------------------------------------------------", []),
  ?INFO_MSG("------------------------------------------------------", []),
  ?INFO_MSG("------------------------------------------------------", []),
  ?INFO_MSG("Attrs ~p~n", [Attrs]),
  Body = "prova messaggio",%fxml:get_attr_s(<<"body">>, Attrs),
  ?INFO_MSG("Message Body ~p~n",[Body]),
  Token = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
  PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
  FromJid = [From#jid.luser, "@", From#jid.lserver],
  ToJid = [To#jid.luser, "@", To#jid.lserver],

  Sep = "&",
  Post = [
    "to=", ToJid, Sep,
    "from=", FromJid, Sep,
    "body=", url_encode(binary_to_list(Body)), Sep,
    "access_token=", Token],
  ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),

  httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)}, [], []).


%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H | T]) when is_list(H) ->
  [url_encode(H) | url_encode(T)];
url_encode([H | T]) ->
  if
    H >= $a, $z >= H ->
      [H | url_encode(T)];
    H >= $A, $Z >= H ->
      [H | url_encode(T)];
    H >= $0, $9 >= H ->
      [H | url_encode(T)];
    H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
      [H | url_encode(T)];
    true ->
      case integer_to_hex(H) of
        [X, Y] ->
          [$%, X, Y | url_encode(T)];
        [X] ->
          [$%, $0, X | url_encode(T)]
      end
  end;

url_encode([]) ->
  [].

integer_to_hex(I) ->
  case catch erlang:integer_to_list(I, 16) of
    {'EXIT', _} -> old_integer_to_hex(I);
    Int -> Int
  end.

old_integer_to_hex(I) when I < 10 ->
  integer_to_list(I);

old_integer_to_hex(I) when I < 16 ->
  [I - 10 + $A];

old_integer_to_hex(I) when I >= 16 ->
  N = trunc(I / 16),
  old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).


mod_opt_type(auth_token) -> fun(A) -> A end;
mod_opt_type(post_url) -> fun(A) -> A end;
mod_opt_type(_) -> [auth_token, post_url].
