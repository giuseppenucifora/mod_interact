%%%----------------------------------------------------------------------

%%% File    : mod_available_post.erl
%%% Author  : Adam Duke <adam.v.duke@gmail.com>
%%% Purpose : Forward presence notifications to a url
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

-module(mod_available_post).
-author('adam.v.duke@gmail.com').

-behaviour(gen_mod).

-export([start/2,
  init/2,
  stop/1,
  send_available_notice/4,
  mod_opt_type/1]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

start(Host, Opts) ->
  Version = "0.1",
  ?INFO_MSG("Starting mod_available_post v.~s", [Version]),
  register(?PROCNAME, spawn(?MODULE, init, [Host, Opts])),
  ok.

%%% set_presence_hook(User, Server, Resource, Packet) -> none
init(Host, _Opts) ->
  inets:start(),
  ssl:start(),
  ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, send_available_notice, 10),
  ok.

stop(Host) ->
  ?INFO_MSG("Stopping mod_available_post", []),
  ejabberd_hooks:delete(set_presence_hook, Host,
    ?MODULE, send_available_notice, 10),
  ok.

send_available_notice(User, Server, _Resource, _Packet) ->
  Token = gen_mod:get_module_opt(Server, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
  PostUrl = gen_mod:get_module_opt(Server, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
  if (Token /= "") ->
    Sep = "&",
    Post = [
      "jabber_id=", User, Sep,
      "access_token=", Token],
    ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
    httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)}, [], [{sync, false}]),
    ok;
    true ->
      ok
  end.


mod_opt_type(auth_token) -> fun(A) -> A end;
mod_opt_type(post_url) -> fun(A) -> A end;
mod_opt_type(_) -> [auth_token, post_url].
