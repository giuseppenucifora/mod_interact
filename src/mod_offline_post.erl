%%%----------------------------------------------------------------------

%%% File    : mod_offline_post.erl
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

-module(mod_offline_post).
-author('rgeorge@midnightweb.net').

-behaviour(gen_mod).

-export([start/2,
  init/2,
  stop/1,
  send_notice/3,
  mod_opt_type/1]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

start(Host, Opts) ->
  Version = "0.1",
  ?INFO_MSG("Starting mod_offline_post v.~s", [Version]),
  register(?PROCNAME, spawn(?MODULE, init, [Host, Opts])),
  ok.

init(Host, _Opts) ->
  inets:start(),
  ssl:start(),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_notice, 10),
  ok.

stop(Host) ->
  ?INFO_MSG("Stopping mod_offline_post", []),
  ejabberd_hooks:delete(user_send_packet, Host,
    ?MODULE, send_notice, 10),
  ok.

send_notice(From, To, Packet) ->
  ?INFO_MSG("mod_offline_post called send_notice", []),
  %Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
  %Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
  %Token = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, fun(S) ->
  %  iolist_to_binary(S) end, list_to_binary("")),
  %PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, fun(S) ->
  %  iolist_to_binary(S) end, list_to_binary("")),
  %Format = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, body_format, fun(S) ->
  %  iolist_to_binary(S) end, iolist_to_binary("")),
  %OfflineMessageCount = get_queue_length(To#jid.luser, To#jid.lserver),
  ?INFO_MSG("From ~s", [From]),
  ?INFO_MSG("To ~s", [To]),
  ?INFO_MSG("Packet ~s", [Packet]),
  ok.

%%if ((Type == <<"chat">>) or (Type == <<"groupchat">>)) and (Body /= <<"">>) ->
%%    Post = case Format of
%%               <<"post">> -> Sep = "&",
%%                   ["to=", To#jid.luser, Sep,
%%                       "from=", From#jid.luser, Sep,
%%                       "body=", url_encode(binary_to_list(Body)), Sep,
%%                       "access_token=", Token, Sep,
%%                       "offline_message_count=", integer_to_list(OfflineMessageCount)];
%%               _ -> Data = [{"to", To#jid.luser},
%%                   {"from", From#jid.luser},
%%                   {"body", Body},
%%                   {"access_token", Token},
%%                   {"offline_message_count", OfflineMessageCount}],
%%                   mochijson2:encode({struct, Data})
%%           end,
%%    ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
%%    httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
%%    ok;
%%    true -> ok
%%end.


%% Get number of offline messages for a user
get_queue_length(LUser, LServer) ->
  get_queue_length(LUser, LServer, gen_mod:db_type(LServer, mod_offline)).

get_queue_length(LUser, LServer, mnesia) ->
  length(mnesia:dirty_read({offline_msg, {LUser, LServer}}));

get_queue_length(LUser, LServer, riak) ->
  case ejabberd_riak:count_by_index(offline_msg, <<"us">>, {LUser, LServer}) of
    {ok, N} -> N;
    _ -> 0
  end;

get_queue_length(LUser, LServer, odbc) ->
  Username = ejabberd_odbc:escape(LUser),
  case catch ejabberd_odbc:sql_query(LServer, [<<"select count(*) from spool  where username='">>, Username, <<"';">>]) of
    {selected, [_], [[SCount]]} -> jlib:binary_to_integer(SCount);
    _ -> 0
  end.

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
