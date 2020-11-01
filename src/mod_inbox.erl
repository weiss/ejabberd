%%%----------------------------------------------------------------------
%%% File    : mod_inbox.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Inbox (XEP-0430)
%%% Created : 18 Oct 2020 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%% Sponsored by Tradelane Solutions <https://simplo.app>.
%%%
%%% ejabberd, Copyright (C) 2020 Tradelane Solutions
%%%           Copyright (C) 2020 ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_inbox).
-author('holger@zedat.fu-berlin.de').
-protocol({xep, 430, '0.2.0'}).

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).

%% ejabberd_hooks callbacks.
-export([disco_sm_features/5, user_send_packet/1, user_receive_packet/1,
	 unread_message_count/2, remove_user/2]).

%% gen_iq_handler callback.
-export([process_iq/1]).

-include("logger.hrl").
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(INBOX_COUNTER_CACHE, inbox_counter_cache).
-define(NS_INBOX_1, <<"urn:xmpp:inbox:1">>). % TODO: Move to 'xmpp'.

-type c2s_state() :: ejabberd_c2s:state().

-callback init(binary(), gen_mod:opts())
          -> any().
-callback store(jid(), jid(), binary(), binary(), integer(), message())
          -> ok | {error, db_failure}.
-callback maybe_reset_unread(jid(), jid(), binary())
          -> boolean() | {error, db_failure}.
-callback reset_unread(jid(), jid())
          -> ok | {error, db_failure}.
-callback get_unread_total(jid())
          -> {unread, non_neg_integer} | {error, db_failure}.
-callback remove_user(binary(), binary())
          -> any().
-callback delete_old_inboxes(binary() | global, integer())
          -> any().

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    case Mod:init(Host, Opts) of
	ok ->
	    init_cache(Mod, Host, Opts),
	    register_iq_handlers(Host),
	    register_hooks(Host);
	Err ->
	    Err
    end.

-spec stop(binary()) -> ok.
stop(Host) ->
    unregister_hooks(Host),
    unregister_iq_handlers(Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [{mod_mam, hard}].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(db_type) ->
    econf:db_type(?MODULE);
mod_opt_type(use_cache) ->
    econf:bool();
mod_opt_type(cache_size) ->
    econf:pos_int(infinity);
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)}, % TODO: Support Mnesia.
     {use_cache, ejabberd_option:use_cache(Host)},         % TODO: Support caching.
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_life_time, ejabberd_option:cache_life_time(Host)}].

mod_doc() ->
    #{desc =>
	  ?T("This module offers XMPP clients a mechanism to discover a list "
	     "of ongoing conversations and their state as specified in "
	     "https://xmpp.org/extensions/xep-0430.html[XEP-0430: Inbox]."),
      opts =>
	  [{db_type,
	    #{value => "mnesia | sql",
	      desc =>
		  ?T("Same as top-level 'default_db' option, but applied to this module only.")}},
	   {use_cache,
	    #{value => "true | false",
	      desc =>
		  ?T("Same as top-level 'use_cache' option, but applied to this module only.")}},
	   {cache_size,
	    #{value => "pos_integer() | infinity",
	      desc =>
		  ?T("Same as top-level 'cache_size' option, but applied to this module only.")}},
	   {cache_life_time,
	    #{value => "timeout()",
	      desc =>
		  ?T("Same as top-level 'cache_life_time' option, but applied to this module only.")}}]}.

%%--------------------------------------------------------------------
%% Register/unregister hooks.
%%--------------------------------------------------------------------
-spec register_hooks(binary()) -> ok.
register_hooks(Host) ->
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       disco_sm_features, 50),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 100),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       user_receive_packet, 100),
    ejabberd_hooks:add(unread_message_count, Host, ?MODULE,
		       unread_message_count, 50).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  disco_sm_features, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
			  user_send_packet, 100),
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE,
			  user_receive_packet, 100),
    ejabberd_hooks:delete(unread_message_count, Host, ?MODULE,
			  unread_message_count, 50).

%%--------------------------------------------------------------------
%% Service discovery.
%%--------------------------------------------------------------------
-spec disco_sm_features(mod_disco:features_acc(), jid(), jid(), binary(),
			   binary()) -> mod_disco:features_acc().
disco_sm_features(empty, From, To, Node, Lang) ->
    disco_sm_features({result, []}, From, To, Node, Lang);
disco_sm_features({result, OtherFeatures},
		  #jid{luser = U, lserver = S},
		  #jid{luser = U, lserver = S}, <<"">>, _Lang) ->
    {result, [?NS_INBOX_1 | OtherFeatures]};
disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%--------------------------------------------------------------------
%% IQ handlers.
%%--------------------------------------------------------------------
-spec register_iq_handlers(binary()) -> ok.
register_iq_handlers(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_INBOX_1, ?MODULE,
				  process_iq).

-spec unregister_iq_handlers(binary()) -> ok.
unregister_iq_handlers(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_INBOX_1).

-spec process_iq(iq()) -> iq().
process_iq(#iq{type = set,
	       lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_iq(#iq{type = get,
	       from = #jid{lserver = LServer} = _From,
	       to = #jid{lserver = LServer},
	       lang = Lang,
	       sub_els = [_ | _]} = IQ) -> % TODO
    Txt = ?T("Inbox support is work in progress"),
    xmpp:make_error(IQ, xmpp:err_feature_not_implemented(Txt, Lang));
process_iq(#iq{type = get,
	       lang = Lang} = IQ) ->
    Txt = ?T("The query is only allowed from local users"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang)).

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec user_send_packet(Acc) -> Acc
      when Acc :: {stanza() | drop, c2s_state()}.
user_send_packet({#message{to = Peer} = Msg, #{jid := JID}} = Acc) ->
    case find_marker(Msg) of
	{displayed, ID} ->
	    case maybe_reset_unread(JID, Peer, ID) of
		true ->
		    ?DEBUG("Reset unread count for ~s with ~s (ID: ~s)",
			   [jid:encode(JID), jid:encode(Peer), ID]);
		_ -> % Either false or {error, _}, the latter was logged.
		    ?DEBUG("Message not found for ~s with ~s (ID: ~s)",
			   [jid:encode(JID), jid:encode(Peer), ID])
	    end;
	none ->
	    case is_instant_msg(Msg) of
		true ->
		    ?DEBUG("Reset unread count for ~s with ~s",
			   [jid:encode(JID), jid:encode(Peer)]),
		    reset_unread(JID, Peer);
		false ->
		    ?DEBUG("Won't reset unread count for ~s with ~s",
			   [jid:encode(JID), jid:encode(Peer)])
	    end
    end,
    Acc;
user_send_packet(Acc) ->
    Acc.

-spec user_receive_packet(Acc) -> Acc
      when Acc :: {stanza() | drop, c2s_state()}.
user_receive_packet({#message{from = Peer, id = MsgID,
			      meta = #{mam_archived := true,
				       stanza_id := MamID}} = Msg,
		     #{jid := JID}} = Acc) ->
    case is_instant_msg(Msg) of
	true ->
	    ?DEBUG("Adding message from ~s to inbox of ~s",
		   [jid:encode(Peer), jid:encode(JID)]),
	    _ = store(JID, Peer, MsgID, MamID, Msg); % Any errors were logged.
	false ->
	    ?DEBUG("Won't add message from ~s to inbox of ~s",
		   [jid:encode(Peer), jid:encode(JID)])
    end,
    Acc;
user_receive_packet(Acc) ->
    Acc.

-spec unread_message_count(non_neg_integer() | undefined, jid())
      -> non_neg_integer() | undefined.
unread_message_count(_Acc, #jid{luser = LUser, lserver = LServer} = JID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:lookup(
	      ?INBOX_COUNTER_CACHE, {LUser, LServer},
	      fun() ->
		      case Mod:get_unread_total(JID) of
			  {unread, Count} ->
			      {cache, Count};
			  {error, db_failure} -> % Error was logged.
			      {nocache, undefined}
		      end
	      end);
	false ->
	    case Mod:get_unread_total(JID) of
		{unread, Count} ->
		    Count;
		{error, db_failure} -> % Error was logged.
		    undefined
	    end
    end.

-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer),
    flush_cache(Mod, LUser, LServer).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec store(jid(), jid(), binary(), integer(), message())
      -> ok | {error, db_failure}.
store(#jid{luser = LUser, lserver = LServer} = JID, Peer, MsgID, MamID, Msg) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:incr(?INBOX_COUNTER_CACHE, {LUser, LServer}, 1,
			   cache_nodes(Mod, LServer));
	false ->
	    ok
    end,
    Mod:store(JID, Peer, MsgID, integer_to_binary(MamID), _TS = MamID, Msg).

-spec find_marker(message()) -> {displayed, binary()} | none.
find_marker(Msg) ->
    case xmpp:get_subtag(Msg, #mark_displayed{}) of
	#mark_displayed{id = ID} ->
	    {displayed, ID};
	false ->
	    none
    end.

-spec maybe_reset_unread(jid(), jid(), binary())
      -> boolean() | {error, db_failure}.
maybe_reset_unread(#jid{luser = LUser, lserver = LServer} = JID, Peer, ID) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:maybe_reset_unread(JID, Peer, ID) of
	true ->
	    flush_cache(Mod, LUser, LServer),
	    true;
	Result ->
	    Result
    end.

-spec reset_unread(jid(), jid()) -> ok | {error, db_failure}.
reset_unread(#jid{luser = LUser, lserver = LServer} = JID, Peer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    flush_cache(Mod, LUser, LServer),
    Mod:reset_unread(JID, Peer).

-spec is_instant_msg(message()) -> boolean().
is_instant_msg(#message{body = Body} = Msg) ->
    case xmpp:get_text(Body) of
	Text when byte_size(Text) > 0 ->
	    true;
	<<>> ->
	    body_is_encrypted(Msg)
    end.

-spec body_is_encrypted(message()) -> boolean().
body_is_encrypted(#message{sub_els = MsgEls}) ->
    case lists:keyfind(<<"encrypted">>, #xmlel.name, MsgEls) of
	#xmlel{children = EncEls} ->
	    lists:keyfind(<<"payload">>, #xmlel.name, EncEls) /= false;
	false ->
	    false
    end.

%%--------------------------------------------------------------------
%% Caching.
%%--------------------------------------------------------------------
-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 2) of
	true ->
	    Mod:use_cache(Host);
	false ->
	    mod_inbox_opt:use_cache(Host)
    end.

-spec init_cache(module(), binary(), gen_mod:opts()) -> any().
init_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
	true ->
	    ets_cache:new(?INBOX_COUNTER_CACHE, cache_opts(Opts));
	false ->
	    ets_cache:delete(?INBOX_COUNTER_CACHE)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true ->
	    Mod:cache_nodes(Host);
	false ->
	    ejabberd_cluster:get_nodes()
    end.

-spec flush_cache(module(), binary(), binary()) -> ok.
flush_cache(Mod, LUser, LServer) ->
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:delete(?INBOX_COUNTER_CACHE, {LUser, LServer},
			     cache_nodes(Mod, LServer));
	false ->
	    ok
    end.

-spec cache_opts(gen_mod:opts()) -> proplists:proplist().
cache_opts(Opts) ->
    MaxSize = mod_inbox_opt:cache_size(Opts),
    LifeTime = mod_inbox_opt:cache_life_time(Opts),
    [{max_size, MaxSize}, {life_time, LifeTime}, {cache_missed, false}].
