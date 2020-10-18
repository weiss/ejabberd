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
-export([disco_sm_features/5]).

%% gen_iq_handler callback.
-export([process_iq/1]).

-include("logger.hrl").
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(NS_INBOX_1, <<"urn:xmpp:inbox:1">>). % TODO: Move to 'xmpp'.

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _Opts) ->
    register_iq_handlers(Host),
    register_hooks(Host).

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
mod_opt_type(cache_missed) ->
    econf:bool();
mod_opt_type(cache_life_time) ->
    econf:timeout(second, infinity).

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)}, % TODO: Support Mnesia.
     {use_cache, ejabberd_option:use_cache(Host)},         % TODO: Support caching.
     {cache_size, ejabberd_option:cache_size(Host)},
     {cache_missed, ejabberd_option:cache_missed(Host)},
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
	   {cache_missed,
	    #{value => "true | false",
	      desc =>
		  ?T("Same as top-level 'cache_missed' option, but applied to this module only.")}},
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
		       disco_sm_features, 50).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  disco_sm_features, 50).

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
