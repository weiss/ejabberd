%%%----------------------------------------------------------------------
%%% File    : mod_push.erl
%%% Author  : Christian Ulrich <christian@rechenwerk.net>
%%% Purpose : Push Notifications (XEP-0357)
%%% Created : 22 Dec 2014 by Christian Ulrich <christian@rechenwerk.net>
%%%
%%% Modified by Holger Weiss <holger@zedat.fu-berlin.de> in 2016.
%%%
%%%
%%% ejabberd, Copyright (C) 2014-2016  ProcessOne
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

-module(mod_push).
-author('christian@rechenwerk.net').
-behaviour(gen_mod).

-export([start/2, stop/1,
	 mod_opt_type/1,
	 process_iq/3,
	 ping/1,
	 noop/1,
	 on_user_available/1,
	 on_unset_presence/4,
	 on_session_pending/4,
	 on_session_resumed/4,
	 on_filter_stanza/4,
	 on_store_offline_message/3,
	 on_store_mam_message/6,
	 on_disco_sm_features/5,
	 on_disco_sm_identity/5,
	 on_remove_user/2]).

-include("logger.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

-define(NS_PUSH, <<"urn:xmpp:push:0">>).
-define(NS_PUSH_SUMMARY, <<"urn:xmpp:push:summary">>).
-define(NS_PUSH_OPTIONS, <<"urn:xmpp:push:options">>).
-define(NS_PUBLISH_OPTIONS,
	<<"http://jabber.org/protocol/pubsub#publish-options">>).

-define(INCLUDE_SENDERS_DEFAULT, false).
-define(INCLUDE_MSG_COUNT_DEFAULT, false).
-define(INCLUDE_SUBSCR_COUNT_DEFAULT, false).
-define(INCLUDE_MSG_BODIES_DEFAULT, false).

-define(MAX_INT, 4294967295).
-define(PUSH_TIMEOUT, 24 * 60 * 60 * 1000).
-define(PUSH_PING_INTERVAL, 60 * 60 * 1000).

%%% TODO:
%%%
%%% - Let c2s process hibernate during wait_for_resume.
%%% - Make things configurable.
%%% - Clean things up.
%%% - Add SQL support.
%%% - Add test cases.

%%------------------------------------------------------------------------
%% xdata-form macros
%%------------------------------------------------------------------------

-define(VVALUE(Val),
	(#xmlel{name = <<"value">>,
		children = [{xmlcdata, Val}]})).

-define(VFIELD(Var, Val),
	(#xmlel{name = <<"field">>,
		attrs = [{<<"var">>, Var}],
		children = vvaluel(Val)})).

-define(TVFIELD(Type, Var, Vals),
	(#xmlel{name = <<"field">>,
		attrs = [{<<"type">>, Type}, {<<"var">>, Var}],
		children = lists:foldl(fun(Val, FieldAcc) ->
					       vvaluel(Val) ++ FieldAcc
				       end, [], Vals)})).

-define(HFIELD(Val), ?TVFIELD(<<"hidden">>, <<"FORM_TYPE">>, [Val])).

-define(ITEM(Fields),
	(#xmlel{name = <<"item">>,
		children = Fields})).

%%------------------------------------------------------------------------

-record(subscription, {resource :: binary(),
		       node :: binary(),
		       reg_type :: reg_type(),
		       resume_timeout :: non_neg_integer(),
		       pending_since :: integer(),
		       ping_timer :: timer:tref(),
		       push_timer :: timer:tref(),
		       state = online :: state()}).

-record(push_user, {bare_jid :: bare_jid() | {binary() | '_', binary()},
		    subscriptions :: [subscription()] | '_',
		    config :: user_config() | '_',
		    payload = [] :: payload() | '_'}).

-type bare_jid() :: {binary(), binary()}.
-type payload_key() ::
	'last-message-sender' | 'last-subscription-sender' | 'message-count' |
	'pending-subscription-count' | 'last-message-body'.
-type payload_value() :: binary() | integer().
-type payload() :: [{payload_key(), payload_value()}].
-type reg_type() :: {local_reg, binary(), binary() | undefined} | % pubsub host, secret
		    {remote_reg, jid(), {binary(), binary()} | undefined}.  % pubsub host, secret
-type state() :: online | pending | waiting | offline.
-type subscription() :: #subscription{}.
-type user_config_option() ::
	'include-senders' | 'include-message-count' | 'include-subscription-count' |
	'include-message-bodies'.
-type user_config() :: [{user_config_option(), boolean()}].

%------------------------------------------------------------------------
%% gen_mod callbacks
%%------------------------------------------------------------------------


start(Host, _Opts) ->
    mnesia:create_table(push_user,
			[{disc_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, push_user)}]),
    % add_table_copy
    UserFields = record_info(fields, push_user),
    case mnesia:table_info(push_user, attributes) of
	UserFields -> ok;
	_ -> mnesia:transform_table(push_user, ignore, UserFields)
    end,
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PUSH, ?MODULE,
				  process_iq, one_queue),
    ejabberd_hooks:add(user_available_hook, Host, ?MODULE,
		       on_user_available, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
		       on_unset_presence, 50),
    ejabberd_hooks:add(c2s_session_pending, Host, ?MODULE,
		       on_session_pending, 50),
    ejabberd_hooks:add(c2s_session_resumed, Host, ?MODULE,
		       on_session_resumed, 50),
    ejabberd_hooks:add(csi_filter_stanza, Host, ?MODULE,
		       on_filter_stanza, 1000),
    ejabberd_hooks:add(store_offline_message, Host, ?MODULE,
		       on_store_offline_message, 50),
    ejabberd_hooks:add(store_mam_message, Host, ?MODULE,
		       on_store_mam_message, 50),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       on_disco_sm_features, 50),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE,
		       on_disco_sm_identity, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       on_remove_user, 50),
    notify_previous_users(Host).

%%------------------------------------------------------------------------


stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUSH),
    ejabberd_hooks:delete(user_available_hook, Host, ?MODULE,
			  on_user_available, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE,
			  on_unset_presence, 50),
    ejabberd_hooks:delete(c2s_session_pending, Host, ?MODULE,
			  on_session_pending, 50),
    ejabberd_hooks:delete(c2s_session_resumed, Host, ?MODULE,
			  on_session_resumed, 50),
    ejabberd_hooks:delete(csi_filter_stanza, Host, ?MODULE,
			  on_filter_stanza, 1000),
    ejabberd_hooks:delete(store_offline_message, Host, ?MODULE,
			  on_store_offline_message, 50),
    ejabberd_hooks:delete(store_mam_message, Host, ?MODULE,
			  on_store_mam_message, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  on_disco_sm_features, 50),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE,
			  on_disco_sm_identity, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  on_remove_user, 50).

%%------------------------------------------------------------------------

mod_opt_type(_) -> [].

%%------------------------------------------------------------------------
%% ejabberd_hooks callbacks
%%------------------------------------------------------------------------

on_user_available(#jid{luser = LUser,
		       lserver = LServer,
		       lresource = LResource}) ->
    ?DEBUG("Session created", []),
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[] ->
	    ok;
	[#push_user{subscriptions = Subscrs} = PushUser] ->
	    Subscrs1 = set_state(online, LResource, Subscrs),
	    mnesia:dirty_write(
	      PushUser#push_user{payload = [],
				 subscriptions = Subscrs1})
    end.

on_unset_presence(User, Server, Resource, _Status) ->
    ?DEBUG("Session closed", []),
    JID = jid:make(User, Server, Resource),
    case get_subscription(JID) of
	not_subscribed ->
	    ok;
	Subscr ->
	    NewSubscr = start_ping_timer(Subscr, JID),
	    set_subscription(JID, NewSubscr#subscription{state = offline})
    end.

on_session_pending(C2SState, _SID,
		   #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
		   _Info) ->
    ?DEBUG("Session of ~s is now pending", [jid:to_string(JID)]),
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[] ->
	    ?DEBUG("User ~s@~s has no subscriptions", [LUser, LServer]),
	    C2SState;
	[#push_user{subscriptions = Subscrs} = PushUser] ->
	    case get_subscription(JID) of
		not_subscribed ->
		    ?DEBUG("Client ~s is not subscribed", [jid:to_string(JID)]),
		    C2SState;
		Subscr ->
		    ?DEBUG("Client ~s is subscribed", [jid:to_string(JID)]),
		    NotMatchingSubscrs =
			lists:filter(
			  fun(S) -> S#subscription.resource =/= LResource end,
			  Subscrs),
		    Subscrs0 = [start_ping_timer(Subscr, JID) | NotMatchingSubscrs],
		    case ejabberd_c2s:get_queued_stanzas(C2SState) of
			[] ->
			    ?DEBUG("Setting state to pending: ~p", [Subscrs0]),
			    Subscrs1 = set_state(pending, LResource, Subscrs0),
			    Timeout = ejabberd_c2s:get_resume_timeout(C2SState),
			    ?DEBUG("Session resume timeout: ~p", [Timeout]),
			    Subscrs2 = set_resume_timeout(LResource, Subscrs1, Timeout),
			    mnesia:dirty_write(
			      PushUser#push_user{payload = [],
						 subscriptions = Subscrs2}),
			    C2SState1 = ejabberd_c2s:set_csi_state(C2SState, inactive),
			    ?DEBUG("Setting resume timeout to: ~p", [?PUSH_TIMEOUT]),
			    C2SState2 = ejabberd_c2s:set_resume_timeout(C2SState1,
									?PUSH_TIMEOUT),
			    C2SState2;
			Stanzas ->
			    ?DEBUG("Setting state to waiting: ~p", [Subscrs0]),
			    Subscrs1 = set_state(waiting, LResource, Subscrs0),
			    mnesia:dirty_write(
			      PushUser#push_user{payload = [],
						 subscriptions = Subscrs1}),
			    dispatch(Stanzas, JID, waiting),
			    ejabberd_c2s:set_csi_state(C2SState, inactive)
		    end
	    end
    end.

on_session_resumed(C2SState, _SID,
		   #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
		   _Info) ->
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[] ->
	    C2SState;
	[#push_user{subscriptions = Subscrs} = PushUser] ->
	    case get_subscription(JID) of
		not_subscribed ->
		    ?DEBUG("Client ~s is not subscribed", [jid:to_string(JID)]),
		    C2SState;
		Subscr ->
		    ?DEBUG("Client ~s is subscribed", [jid:to_string(JID)]),
		    NotMatchingSubscrs =
			lists:filter(
			  fun(S) -> S#subscription.resource =/= LResource end,
			  Subscrs),
		    Subscrs0 = [stop_ping_timer(stop_push_timer(Subscr)) | NotMatchingSubscrs],
		    ?DEBUG("Setting state to online: ~p", [Subscrs0]),
		    NewSubscrs = set_state(online, LResource, Subscrs0),
		    ?DEBUG("Set state to online: ~p", [NewSubscrs]),
		    ?DEBUG("WRITING PushUser: ~p", [NewSubscrs]),
		    mnesia:dirty_write(PushUser#push_user{payload = [],
							  subscriptions = NewSubscrs}),
		    case get_resume_timeout(LResource, Subscrs) of
			{value, Timeout, _PendingSince} ->
			    ?DEBUG("RESUME_TIMEOUT set c2s: ~p", [Timeout]),
			    ejabberd_c2s:set_resume_timeout(C2SState, Timeout);
			undefined ->
			    ?DEBUG("RESUME_TIMEOUT set c2s: NONE", []),
			    C2SState
		    end
		    %% No need to reset the CSI state, as ejabberd_c2s always sets that
		    %% to 'active' on session resumption.
	    end
    end.

on_filter_stanza({_C2SState, []} = Acc, _Host, _JID, _Stanza) -> Acc;
on_filter_stanza({C2SState, Stanzas} = Acc, _Host,
		 #jid{luser = LUser,
		      lserver = LServer,
		      lresource = LResource} = JID, _Stanza) ->
    ?DEBUG("A stanza is sent to ~s", [jid:to_string(JID)]),
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[] ->
	    ?DEBUG("User ~s@~s has no subscriptions", [LUser, LServer]),
	    Acc;
	[#push_user{subscriptions = Subscrs} = PushUser] ->
	    case get_subscription(JID, Subscrs) of
		not_subscribed ->
		    ?DEBUG("JID ~s is not subscribed", [jid:to_string(JID)]),
		    Acc;
		#subscription{state = pending} ->
		    ?DEBUG("Going to push a notification to ~s",
			   [jid:to_string(JID)]),
		    dispatch(Stanzas, JID, pending),
		    case get_resume_timeout(LResource, Subscrs) of
			{value, Timeout, PendingSince} ->
			    ?DEBUG("Setting state to waiting: ~p", [Subscrs]),
			    Subscrs1 = set_state(waiting, LResource, Subscrs),
			    Now = p1_time_compat:monotonic_time(milli_seconds),
			    NewTimeout = Timeout + (Now - PendingSince),
			    ?DEBUG("WRITING PushUser: ~p", [Subscrs1]),
			    mnesia:dirty_write(
			      PushUser#push_user{subscriptions = Subscrs1}),
			    ?DEBUG("RESUME_TIMEOUT set c2s: ~p (was: ~p)", [NewTimeout, Timeout]),
			    {ejabberd_c2s:set_resume_timeout(C2SState, NewTimeout),
			     Stanzas};
			undefined ->
			    Acc
		    end;
		#subscription{state = waiting} ->
		    dispatch(Stanzas, JID, waiting),
		    Acc;
		_ ->
		    Acc
	    end
    end.

on_store_offline_message(Message, _From, JID) ->
    ?DEBUG("A stanza to ~s is stored offline", [jid:to_string(JID)]),
    dispatch([Message], JID, offline),
    Message.

on_store_mam_message(Message, LUser, LServer, _Peer, chat, recv) ->
    JID = jid:make(LUser, LServer, <<>>),
    ?DEBUG("A message for ~s is stored in MAM", [jid:to_string(JID)]),
    case was_offline_message(Message, LServer) of
	true ->
	    ?DEBUG("This was an offline message", []);
	false ->
	    dispatch([Message], JID, offline)
    end,
    Message;
on_store_mam_message(Message, _LUser, _LServer, _Peer, _Type, _Dir) -> Message.

was_offline_message(Message, LServer) ->
    lists:any(fun(#xmlel{attrs = Attrs,
			 children = [{xmlcdata, <<"Offline Storage">>}]}) ->
		      fxml:get_attr_s(<<"from">>, Attrs) =:= LServer;
		 (_) ->
		      false
	      end, fxml:get_subtags_with_xmlns(Message, <<"delay">>, ?NS_DELAY)).

%%------------------------------------------------------------------------


enable(_UserJid, _PubsubJid, undefined, _XDataForms) ->
    {error, ?ERR_NOT_ACCEPTABLE};

enable(_UserJid, _PubsubJid, <<"">>, _XDataForms) ->
    {error, ?ERR_NOT_ACCEPTABLE};

enable(#jid{luser = LUser, lserver = LServer, lresource = LResource},
       PubsubJid, Node, XDataForms) ->
    ParsedSecret =
	parse_form(XDataForms, ?NS_PUBLISH_OPTIONS, [], [{single, <<"secret">>}]),
    ?DEBUG("+++++ ParsedSecret = ~p", [ParsedSecret]),
    Secret = case ParsedSecret of
		 not_found -> undefined;
		 error -> error;
		 {result, [undefined]} ->
		     case parse_form(XDataForms, ?NS_PUBLISH_OPTIONS, [], [{single, <<"token">>}]) of
			 not_found -> undefined;
			 error -> error;
			 {result, [T]} ->
			     ?DEBUG("+++++ Token (ChatSecure?) = ~p", [T]),
			     {<<"token">>, T}
		     end;
		 {result, [S]} ->
		     {<<"secret">>, S}
	     end,
    case Secret of
	error ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    RegType = {remote_reg, PubsubJid, Secret},
	    Subscr =
		#subscription{resource = LResource,
			      node = Node,
			      reg_type = RegType},
	    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
		[] ->
		    ?DEBUG("+++++ enable: no user found!", []),
		    GConfig = get_global_config(LServer),
		    case make_config(XDataForms, GConfig, enable_disable) of
			error ->
			    {error, ?ERR_NOT_ACCEPTABLE};
			{Config, ChangedOpts} ->
			    %% NewUser will have empty payload
			    NewUser =
				#push_user{bare_jid = {LUser, LServer},
					   subscriptions = [Subscr],
					   config = Config},
			    ?DEBUG("WRITING PushUser: ~p", [[Subscr]]),
			    mnesia:dirty_write(NewUser),
			    case make_config_form(ChangedOpts) of
				[] ->
				    {enabled, ok};
				ResponseForm ->
				    {enabled, ResponseForm}
			    end
		    end;

		[#push_user{subscriptions = Subscriptions,
			    config = OldConfig}] ->
		    ?DEBUG("+++++ enable: found user, config = ~p", [OldConfig]),
		    case make_config(XDataForms, OldConfig, disable_only) of
			error ->
			    {error, ?ERR_NOT_ACCEPTABLE};
			{Config, ChangedOpts} ->
			    FilterNode =
				fun(S) when S#subscription.node =:= Node;
					    S#subscription.resource =:= LResource ->
					false;
				   (_) ->
					true
				end,
			    NewSubscriptions =
				[Subscr|lists:filter(FilterNode, Subscriptions)],
			    %% NewUser will have empty payload
			    NewUser =
				#push_user{bare_jid = {LUser, LServer},
					   subscriptions = NewSubscriptions,
					   config = Config},
			    ?DEBUG("WRITING PushUser: ~p", [NewSubscriptions]),
			    mnesia:dirty_write(NewUser),
			    case make_config_form(ChangedOpts) of
				[] ->
				    {enabled, ok};
				ResponseForm ->
				    {enabled, ResponseForm}
			    end
		    end
	    end
    end.

%%------------------------------------------------------------------------


disable(_From, _PubsubJid, <<"">>) ->
    {error, ?ERR_NOT_ACCEPTABLE};

disable(#jid{luser = LUser, lserver = LServer},
	#jid{lserver = PubsubHost} = PubsubJid, Node) ->
    SubscrPred =
	fun(#subscription{node = N, reg_type = RegT}) ->
		NodeMatching =
		    (Node =:= undefined) or (Node =:= N),
		RegTypeMatching =
		    case RegT of
			{local_reg, P, _} -> P =:= PubsubHost;
			{remote_reg, J, _} ->
			    (J#jid.luser =:= PubsubJid#jid.luser) and
			      (J#jid.lserver =:= PubsubJid#jid.lserver) and
			        (J#jid.lresource =:= PubsubJid#jid.lresource)
		    end,
		NodeMatching and RegTypeMatching
	end,
    case delete_subscriptions({LUser, LServer}, SubscrPred) of
	ok ->
	    {disabled, ok};
	not_found ->
	    {error, ?ERR_ITEM_NOT_FOUND}
    end.

%%------------------------------------------------------------------------


delete_subscriptions({LUser, LServer}, SubscriptionPred) ->
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[] ->
	    not_found;
	[#push_user{subscriptions = Subscriptions} = User] ->
	    {MatchingSubscrs, NotMatchingSubscrs} =
		lists:partition(SubscriptionPred, Subscriptions),
	    case MatchingSubscrs of
		[] ->
		    not_found;
		_ ->
		    ?DEBUG("+++++ Deleting subscriptions for user ~p@~p", [LUser, LServer]),
		    % XXX: Stop timers.
		    case NotMatchingSubscrs of
			[] ->
			    mnesia:dirty_delete({push_user, {LUser, LServer}});
			_ ->
			    UpdatedUser =
				User#push_user{subscriptions = NotMatchingSubscrs},
			    ?DEBUG("WRITING PushUser: ~p", [NotMatchingSubscrs]),
			    mnesia:dirty_write(UpdatedUser)
		    end
	    end
    end.

%%------------------------------------------------------------------------


dispatch(Stanzas, #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
	 State) ->
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[] ->
	    not_subscribed;
	[#push_user{subscriptions = Subscrs,
		    config = Config,
		    payload = OldPayload} = PushUser] ->
	    ?DEBUG("Dispatch: found push user, ~B Stanzas", [length(Stanzas)]),
	    {MatchingSubscrs, NotMatchingSubscrs} =
		if State =:= pending;
		   State =:= waiting ->
			lists:partition(
			  fun(#subscription{state = SubscrS, resource = SubscrR}) ->
				  SubscrS =:= State andalso SubscrR =:= LResource
			  end, Subscrs);
		   State =:= offline ->
			lists:partition(
			  fun(#subscription{state = SubscrS}) ->
				  SubscrS =:= offline
			  end, Subscrs)
		end,
	    if MatchingSubscrs =:= [] ->
		    ?DEBUG("Dispatch: found no subscriptions", []),
		    not_subscribed;
	       true ->
		    ?DEBUG("Dispatch: found subscription(s)", []),
		    {Payload, _StanzasToStore} = make_payload(Stanzas, OldPayload,
							      Config),
		    NewSubscrs =
			lists:foldl(fun(#subscription{reg_type = RegType,
						      node = NodeId} = Subscr, Acc) ->
					    do_dispatch(RegType, {LUser, LServer},
							NodeId, Payload),
					    [start_push_timer(Subscr, JID) | Acc]
				    end, NotMatchingSubscrs, MatchingSubscrs),
		    mnesia:dirty_write(
		      PushUser#push_user{payload = Payload,
					 subscriptions = NewSubscrs})
	    end
    end.

%%------------------------------------------------------------------------


do_dispatch({local_reg, _, _Secret}, _UserBare, _NodeId, _Payload) ->
    throw(not_implemented);

do_dispatch({remote_reg, PubsubHost, Secret}, UserBare, NodeId, Payload) ->
    do_dispatch_remote(UserBare, PubsubHost, NodeId, Payload, Secret),
    ok.

%%------------------------------------------------------------------------


do_dispatch_remote({U, S}, _PubsubJid, _Node, _Payload, undefined) ->
    ?DEBUG("Got no secret for ~s@~s", [U, S]); % Needs to re-enable.
do_dispatch_remote(US, PubsubJid, Node, Payload, Secret) when is_binary(Secret) ->
    do_dispatch_remote(US, PubsubJid, Node, Payload, {<<"secret">>, Secret});
do_dispatch_remote({User, Server}, PubsubJid, Node, Payload, {SecretK, SecretV}) ->
    MakeKey = fun(Atom) -> atom_to_binary(Atom, utf8) end,
    Fields =
	lists:foldl(
	  fun({Key, Value}, Acc) when is_binary(Value) ->
		  [?VFIELD(MakeKey(Key), Value)|Acc];

	     ({Key, Value}, Acc) when is_integer(Value) ->
		  [?VFIELD(MakeKey(Key), integer_to_binary(Value))|Acc]
	  end,
	  [?HFIELD(?NS_PUSH_SUMMARY)],
	  Payload),
    Notification =
	#xmlel{name = <<"notification">>, attrs = [{<<"xmlns">>, ?NS_PUSH}],
	       children =
		   [#xmlel{name = <<"x">>,
			   attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
			   children = Fields}]},
    PubOpts =
	case is_binary(SecretV) of
	    true ->
		[#xmlel{name = <<"publish-options">>,
			children =
			    [#xmlel{name = <<"x">>,
				    attrs = [{<<"xmlns">>, ?NS_XDATA},
					     {<<"type">>, <<"submit">>}],
				    children = [?HFIELD(?NS_PUBLISH_OPTIONS),
						?VFIELD(SecretK, SecretV)] ++
						case SecretK of
						    <<"secret">> ->
							[];
						    <<"token">> ->
							[?VFIELD(<<"endpoint">>, <<"https://push.chatsecure.org/api/v1/messages/">>)]
						end}]}];
	    false -> []
	end,
    Iq =
	#xmlel{name = <<"iq">>, attrs = [{<<"type">>, <<"set">>}, {<<"id">>, randoms:get_string()}],
	       children =
		   [#xmlel{name = <<"pubsub">>, attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
			   children =
			       [#xmlel{name = <<"publish">>, attrs = [{<<"node">>, Node}],
				       children =
					   [#xmlel{name = <<"item">>,
						   children = [Notification]}]}] ++ PubOpts}]},
    ejabberd_router:route(ljid_to_jid({User, Server, <<"">>}), PubsubJid, Iq).

%%------------------------------------------------------------------------


ping(#jid{luser = LUser, lserver = LServer, lresource = LResource} = JID) ->
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[#push_user{bare_jid = BareJid,
		    subscriptions = Subscrs,
		    config = Config,
		    payload = Payload}] ->
	    case [S || #subscription{resource = R} = S <- Subscrs, R =:= LResource] of
		[#subscription{node = Node, reg_type = RegType}] ->
		    ?DEBUG("Pinging ~s", [jid:to_string(JID)]),
		    FilteredPayload = filter_payload(Payload, Config),
		    do_dispatch(RegType, BareJid, Node, FilteredPayload);
		_ ->
		    ?DEBUG("Cannot ping ~s: subscription not found",
			   [jid:to_string(JID)])
	    end;
	[] ->
	    ?DEBUG("Cannot ping ~s: push user not found",
		   [jid:to_string(JID)])
    end,
    ok.


start_ping_timer(#subscription{ping_timer = undefined} = Subscr, JID) ->
    ?DEBUG("Starting ping timer for ~s", [jid:to_string(JID)]),
    {ok, PingTimer} = timer:apply_interval(?PUSH_PING_INTERVAL,
					   ?MODULE, ping, [JID]),
    Subscr#subscription{ping_timer = PingTimer};
start_ping_timer(Subscr, _JID) ->
    Subscr.


start_push_timer(#subscription{push_timer = undefined} = Subscr, JID) ->
    ?DEBUG("Starting push timer for ~s", [jid:to_string(JID)]),
    {ok, PushTimer} = timer:apply_after(?PUSH_TIMEOUT,
					?MODULE, noop, [JID]),
    Subscr#subscription{ping_timer = PushTimer};
start_push_timer(Subscr, _JID) ->
    Subscr.


stop_ping_timer(#subscription{ping_timer = undefined} = Subscr) ->
    Subscr;
stop_ping_timer(#subscription{ping_timer = PingTimer} = Subscr) ->
    ?DEBUG("Stopping ping timer", []),
    timer:cancel(PingTimer),
    Subscr#subscription{ping_timer = undefined}.


stop_push_timer(#subscription{push_timer = undefined} = Subscr) ->
    Subscr;
stop_push_timer(#subscription{push_timer = PushTimer} = Subscr) ->
    ?DEBUG("Stopping push timer", []),
    timer:cancel(PushTimer),
    Subscr#subscription{push_timer = undefined}.

%%------------------------------------------------------------------------


on_remove_user(User, Server) ->
    case mnesia:dirty_read({push_user, {User, Server}}) of
	[#push_user{}] ->
	    % XXX: Cancel timers.
	    mnesia:dirty_delete({push_user, {User, Server}});
	[] ->
	    ok
    end.

%%------------------------------------------------------------------------


notify_previous_users(Host) ->
    MatchHead = #push_user{bare_jid = {'_', Host}, _ = '_'},
    Users = mnesia:dirty_select(push_user, [{MatchHead, [], ['$_']}]),
    lists:foreach(
      fun(#push_user{bare_jid = BareJid,
		     subscriptions = Subscrs,
		     config = Config,
		     payload = Payload}) ->
	      lists:foreach(
		fun(#subscription{node = Node, reg_type = RegType}) ->
			FilteredPayload = filter_payload(Payload, Config),
			do_dispatch(RegType, BareJid, Node, FilteredPayload);
		   (_) ->
			ok
		end,
		Subscrs)
      end,
      Users),
    %% TODO: Don't delete subscriptions here (but remove timer refs).
    lists:foreach(fun mnesia:dirty_delete_object/1, Users).

%%------------------------------------------------------------------------


process_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    JidB = proplists:get_value(<<"jid">>, SubEl#xmlel.attrs),
    Node = proplists:get_value(<<"node">>, SubEl#xmlel.attrs),
    case JidB of
	undefined -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	_ ->
	    case jid:from_string(JidB) of
		error ->
		    IQ#iq{type = error, sub_el = [?ERR_JID_MALFORMED, SubEl]};

		Jid ->
		    case {Type, SubEl} of
			{set, #xmlel{name = <<"enable">>,
				     children = Children}} ->
			    XDataForms = get_xdata_elements(Children),
			    case enable(From, Jid, Node, XDataForms) of
				{enabled, ok} ->
				    IQ#iq{type = result, sub_el = []};

				{enabled, ResponseForm} ->
				    NewSubEl =
					SubEl#xmlel{children = ResponseForm},
				    IQ#iq{type = result, sub_el = [NewSubEl]};

				{error, Error} ->
				    IQ#iq{type = error,
					  sub_el = [Error, SubEl]}
			    end;

			{set, #xmlel{name = <<"disable">>}} ->
			    case disable(From, Jid, Node) of
				{disabled, ok} ->
				    IQ#iq{type = result, sub_el = []};

				{error, Error} ->
				    IQ#iq{type = error,
					  sub_el = [Error, SubEl]}
			    end;

			_ ->
			    ?DEBUG("Received invalid push IQ from ~s",
				   [jid:to_string(From)]),
			    IQ#iq{type = error,
				  sub_el = [?ERR_NOT_ALLOWED, SubEl]}
		    end
	    end
    end.

%%------------------------------------------------------------------------


on_disco_sm_features(empty, _From, _To, <<"">>, _Lang) ->
    ?DEBUG("on_disco_sm_features, returning ~p",
	   [{result, [?NS_PUSH]}]),
    {result, [?NS_PUSH]};

on_disco_sm_features({result, Features}, _From, _To, <<"">>, _Lang) ->
    ?DEBUG("on_disco_sm_features, returning ~p",
	   [{result, [?NS_PUSH|Features]}]),
    {result, [?NS_PUSH|Features]};

on_disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    ?DEBUG("on_disco_sm_features, returning ~p", [Acc]),
    Acc.

%%------------------------------------------------------------------------


on_disco_sm_identity(Acc, From, To, <<"">>, _Lang) ->
    FromL = jid:tolower(From),
    ToL = jid:tolower(To),
    case jid:remove_resource(FromL) of
	ToL ->
	    case mnesia:dirty_read({push_user, {To#jid.luser, To#jid.lserver}}) of
		[] ->
		    make_config_form(get_global_config(To#jid.lserver)) ++ Acc;
		[#push_user{config = Config}] ->
		    make_config_form(Config) ++ Acc
	    end;
	_ ->
	    Acc
    end;

on_disco_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_subscription(#jid{lresource = LResource} = JID, Subscrs) ->
    ?DEBUG("get_sub(~p, ~p)", [JID, Subscrs]),
    case [S || #subscription{resource = R} = S <- Subscrs, R =:= LResource] of
	[] ->
	    not_subscribed;
	[Subscr] ->
	    Subscr
    end.

get_subscription(#jid{luser = LUser, lserver = LServer} = JID) ->
    case mnesia:dirty_read({push_user, {LUser, LServer}}) of
	[] ->
	    not_subscribed;
	[#push_user{subscriptions = Subscrs}] ->
	    get_subscription(JID, Subscrs)
    end.

set_subscription(#jid{luser = LUser, lserver = LServer, lresource = LResource},
		 Subscr) ->
    [#push_user{subscriptions = Subscrs} = PushUser] =
	mnesia:dirty_read({push_user, {LUser, LServer}}),
    NotMatchingSubscrs =
	lists:filter(
	  fun(S) -> S#subscription.resource =/= LResource end,
	  Subscrs),
    ?DEBUG("Storing push user: ~p", [[Subscr | NotMatchingSubscrs]]),
    mnesia:dirty_write(PushUser#push_user{
			 subscriptions = [Subscr | NotMatchingSubscrs]}).

noop(#jid{luser = LUser, lserver = LServer}) ->
    ?DEBUG("WOULD delete push user: ~s@~s", [LUser, LServer]).

%%------------------------------------------------------------------------
%% mod_push utility functions
%%------------------------------------------------------------------------


get_global_config(Host) ->
    [{'include-senders',
      gen_mod:get_module_opt(Host, ?MODULE, include_senders,
			     fun(B) when is_boolean(B) -> B end,
			     ?INCLUDE_SENDERS_DEFAULT)},
     {'include-message-count',
      gen_mod:get_module_opt(Host, ?MODULE, include_message_count,
			     fun(B) when is_boolean(B) -> B end,
			     ?INCLUDE_MSG_COUNT_DEFAULT)},
     {'include-subscription-count',
      gen_mod:get_module_opt(Host, ?MODULE, include_subscription_count,
			     fun(B) when is_boolean(B) -> B end,
			     ?INCLUDE_SUBSCR_COUNT_DEFAULT)},
     {'include-message-bodies',
      gen_mod:get_module_opt(Host, ?MODULE, include_message_bodies,
			     fun(B) when is_boolean(B) -> B end,
			     ?INCLUDE_MSG_BODIES_DEFAULT)}].

%%------------------------------------------------------------------------


make_config(XDataForms, OldConfig, ConfigPrivilege) ->
    %% if a user is allowed to change an option from OldValue to NewValue,
    %% OptionAllowed(OldValue, NewValue) returns true
    OptionAllowed = case ConfigPrivilege of
			disable_only ->
			    fun(true, false) -> true;
			       (Old, New) when Old =:= New -> true;
			       (_, _) -> false
			    end;
			enable_disable ->
			    fun(_, NewValue) when not is_boolean(NewValue) -> false;
			       (_, _) -> true
			    end
		    end,
    AllowedOpts =
	['include-senders', 'include-message-count', 'include-subscription-count',
	 'include-message-bodies'],
    OptionalFields =
	lists:map(
	  fun(Opt) -> {{single, atom_to_binary(Opt, utf8)},
		       fun(B) -> binary_to_boolean(B, error) end}
	  end,
	  AllowedOpts),
    ParseResult = parse_form(XDataForms, ?NS_PUSH_OPTIONS, [], OptionalFields),
    ?DEBUG("ParseResult = ~p", [ParseResult]),
    case ParseResult of
	error -> error;

	not_found -> {OldConfig, []};

	{result, ParsedOptions} ->
	    AnyError =
		lists:any(
		  fun(error) -> true;
		     (_) -> false
		  end,
		  ParsedOptions),
	    case AnyError of
		true -> error;

		false ->
		    lists:foldl(
		      fun({Key, Value}, {ConfigAcc, AcceptedOptsAcc}) ->
			      OldValue = proplists:get_value(Key, OldConfig),
			      AcceptOpt = OptionAllowed(OldValue, Value),
			      case AcceptOpt of
				  true ->
				      {[{Key, Value}|ConfigAcc],
				       [{Key, Value}|AcceptedOptsAcc]};
				  false ->
				      {[{Key, OldValue}|ConfigAcc],
				       AcceptedOptsAcc}
			      end
		      end,
		      {[], []},
		      lists:zip(AllowedOpts, ParsedOptions))
	    end
    end.

%%------------------------------------------------------------------------


%% TODO: Don't return StanzasToStore.

make_payload(Stanzas, Payload, Config) ->
    UpdatePayload =
	fun(NewValues, OldPayload) ->
		lists:foldl(
		  fun({_Key, undefined}, Acc) -> Acc;
		     ({Key, Value}, Acc) -> lists:keystore(Key, 1, Acc, {Key, Value})
		  end,
		  OldPayload,
		  NewValues)
	end,
    MakeNewValues =
	fun(Stanza, OldPayload) ->
		FromS = proplists:get_value(<<"from">>, Stanza#xmlel.attrs),
		case Stanza of
		    #xmlel{name = <<"message">>, children = Children} ->
			BodyPred =
			    fun(#xmlel{name = <<"body">>}) -> true;
			       (_) -> false
			    end,
			NewBody = case lists:filter(BodyPred, Children) of
				      [] -> undefined;
				      [#xmlel{children = [{xmlcdata, CData}]}|_] -> CData
				  end,
			NewMsgCount =
			    case proplists:get_value('message-count', OldPayload, 0) of
				?MAX_INT -> 0;
				C when is_integer(C) -> C + 1
			    end,
			{push_and_store,
			 [{'last-message-body', NewBody},
			  {'last-message-sender', FromS},
			  {'message-count', NewMsgCount}]};

		    #xmlel{name = <<"presence">>, attrs = Attrs} ->
			case proplists:get_value(<<"type">>, Attrs) of
			    <<"subscribe">> ->
				OldSubscrCount =
				    proplists:get_value('pending-subscriptions', OldPayload, 0),
				NewSubscrCount =
				    case OldSubscrCount of
					?MAX_INT -> 0;
					C when is_integer(C) -> C + 1
				    end,
				{push,
				 [{'pending-subscription-count', NewSubscrCount},
				  {'last-subscription-sender', FromS}]};

			    _ ->
				{push, []}
			end;

		    _ -> {push, []}
		end
	end,
    {NewPayload, StanzasToStore} =
	lists:foldl(
	  fun(Stanza, {PayloadAcc, StanzasAcc}) ->
		  case MakeNewValues(Stanza, PayloadAcc) of
		      {push, NewValues} ->
			  {UpdatePayload(NewValues, PayloadAcc), StanzasAcc};

		      {push_and_store, NewValues} ->
			  {UpdatePayload(NewValues, PayloadAcc),
			   [Stanza|StanzasAcc]}
		  end
	  end,
	  {Payload, []}, Stanzas),
    {filter_payload(NewPayload, Config), StanzasToStore}.

%%------------------------------------------------------------------------


filter_payload(Payload, Config) ->
    OptsConfigMapping =
	[{'message-count', 'include-message-count'},
	 {'last-message-sender', 'include-senders'},
	 {'last-subscription-sender', 'include-senders'},
	 {'last-message-body', 'include-message-bodies'},
	 {'pending-subscription-count', 'include-subscription-count'}],
    lists:filter(
      fun({Key, _}) ->
	      ConfigOpt = proplists:get_value(Key, OptsConfigMapping),
	      proplists:get_value(ConfigOpt, Config)
      end,
      Payload).

%%------------------------------------------------------------------------


set_state(State, Resource, Subscrs) ->
    ?DEBUG("Setting push state: ~p, ~p, ~p", [State, Resource, Subscrs]),
    {MatchingSubscrs, NotMatchingSubscrs} =
	lists:partition(
	  fun(S) -> S#subscription.resource =:= Resource end,
	  Subscrs),
    case MatchingSubscrs of
	[] ->
	    Subscrs;
	[S0] ->
	    NewSubscr =
		case State of
		    online ->
			S1 = stop_ping_timer(S0),
			S2 = stop_push_timer(S1),
			S2;
		    _ ->
			S0
		end,
	    [NewSubscr#subscription{state = State} | NotMatchingSubscrs]
    end.

get_resume_timeout(Resource, Subscrs) ->
    R = case lists:keysearch(Resource, #subscription.resource, Subscrs) of
	{value, #subscription{resume_timeout = T, pending_since = P}} when is_integer(T) ->
	    true = is_integer(P), % XXX: Shouldn't be necessary.
	    {value, T, P};
	_ ->
	    undefined
    end,
    ?DEBUG("get_resume_timeout(~s, ~p) = ~p", [Resource, Subscrs, R]),
    R.

set_resume_timeout(Resource, Subscrs, Timeout) ->
    PendingSince = p1_time_compat:monotonic_time(milli_seconds),
    X = lists:keysearch(Resource, #subscription.resource, Subscrs),
    {value, S} = X,
    R = lists:keystore(Resource, #subscription.resource, Subscrs,
		       S#subscription{resume_timeout = Timeout,
				      pending_since = PendingSince}),
    ?DEBUG("set_resume_timeout(~s, ~p, ~p) = ~p", [Resource, Subscrs, Timeout, R]),
    R.

%%------------------------------------------------------------------------
%% general utility functions
%%------------------------------------------------------------------------

vvaluel(Val) ->
    case Val of
	<<>> -> [];
	_ -> [?VVALUE(Val)]
    end.

get_xdata_elements(Elements) ->
    get_xdata_elements(Elements, []).

get_xdata_elements([#xmlel{name = <<"x">>, attrs = Attrs} = H | T], Acc) ->
    case proplists:get_value(<<"xmlns">>, Attrs) of
	?NS_XDATA ->
	    NewAttrs =
		case fxml:get_attr(<<"type">>, Attrs) of
		    {value, _Type} ->
			Attrs;
		    false ->
			[{<<"type">>, <<"submit">>}|Attrs]
		end,
	    get_xdata_elements(T, [H#xmlel{attrs = NewAttrs}|Acc]);
	_ ->
	    get_xdata_elements(T, Acc)
    end;

get_xdata_elements([_ | T], Acc) ->
    get_xdata_elements(T, Acc);

get_xdata_elements([], Acc) ->
    lists:reverse(Acc).

%%------------------------------------------------------------------------


get_xdata_value(FieldName, Fields) ->
    get_xdata_value(FieldName, Fields, undefined).


get_xdata_value(FieldName, Fields, DefaultValue) ->
    case proplists:get_value(FieldName, Fields, [DefaultValue]) of
	[Value] -> Value;
	_ -> error
    end.


get_xdata_values(FieldName, Fields) ->
    get_xdata_values(FieldName, Fields, []).


get_xdata_values(FieldName, Fields, DefaultValue) ->
    proplists:get_value(FieldName, Fields, DefaultValue).

%%------------------------------------------------------------------------


parse_form([], _FormType, _RequiredFields, _OptionalFields) ->
    not_found;
parse_form([XDataForm|T], FormType, RequiredFields, OptionalFields) ->
    case jlib:parse_xdata_submit(XDataForm) of
	invalid -> parse_form(T, FormType, RequiredFields, OptionalFields);
	Fields ->
	    case get_xdata_value(<<"FORM_TYPE">>, Fields) of
		FormType ->
		    GetValues =
			fun({multi, Key}) -> get_xdata_values(Key, Fields);
			   ({single, Key}) -> get_xdata_value(Key, Fields);
			   ({KeyTuple, Convert}) ->
				case KeyTuple of
				    {multi, Key} ->
					Values = get_xdata_values(Key, Fields),
					Converted = lists:foldl(
					    fun(_, error) -> error;
					       (undefined, Acc) -> [undefined|Acc];
					       (B, Acc) ->
						    try [Convert(B)|Acc]
						    catch error:badarg -> error
						    end
					    end,
					    [],
					    Values),
					lists:reverse(Converted);

				    {single, Key} ->
					case get_xdata_value(Key, Fields) of
					    error -> error;
					    undefined -> undefined;
					    Value ->
					       try Convert(Value)
					       catch error:badarg -> error
					       end
					end
				end
			end,
		    RequiredValues = lists:map(GetValues, RequiredFields),
		    OptionalValues = lists:map(GetValues, OptionalFields),
		    RequiredOk =
			lists:all(
			    fun(V) ->
				    (V =/= undefined) and (V =/= []) and (V =/= error)
			    end,
			    RequiredValues),
		    OptionalOk =
			lists:all(fun(V) -> V =/= error end, OptionalValues),
		    case RequiredOk and OptionalOk of
			false -> error;
			true ->
			    {result, RequiredValues ++ OptionalValues}
		    end;

		_ -> parse_form(T, FormType, RequiredFields, OptionalFields)
	    end
    end.

%%------------------------------------------------------------------------


make_config_form(Opts) ->
    Fields =
	[?TVFIELD(<<"boolean">>, atom_to_binary(K, utf8), [boolean_to_binary(V)]) ||
	 {K, V} <- Opts],
    case Fields of
	[] -> [];
	_ ->
	    [#xmlel{name = <<"x">>,
		    attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
		    children = [?HFIELD(?NS_PUSH_OPTIONS)|Fields]}]
    end.

%%------------------------------------------------------------------------


boolean_to_binary(Bool) ->
    case Bool of
	true -> <<"1">>;
	false -> <<"0">>
    end.


binary_to_boolean(Binary, DefaultResult) ->
    binary_to_boolean(Binary, DefaultResult, error).


binary_to_boolean(Binary, DefaultResult, InvalidResult) ->
    case Binary of
	<<"1">> -> true;
	<<"0">> -> false;
	<<"true">> -> true;
	<<"false">> -> false;
	undefined -> DefaultResult;
	_ -> InvalidResult
    end.

%%------------------------------------------------------------------------


ljid_to_jid({LUser, LServer, LResource}) ->
    #jid{user = LUser, server = LServer, resource = LResource,
	 luser = LUser, lserver = LServer, lresource = LResource}.
