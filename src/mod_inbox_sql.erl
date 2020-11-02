%%%----------------------------------------------------------------------
%%% File    : mod_inbox_sql.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
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

-module(mod_inbox_sql).
-author('holger@zedat.fu-berlin.de').
-behaviour(mod_inbox).

%% API
-export([init/2, store/6, maybe_reset_unread/3, reset_unread/2,
	 get_unread_total/1, remove_user/2, delete_old_inboxes/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%--------------------------------------------------------------------
%% API.
%%--------------------------------------------------------------------
-spec init(binary(), gen_mod:opts()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec store(jid(), jid(), binary(), binary(), integer(), message())
      -> ok | {error, db_failure}.
store(#jid{luser = LUser, lserver = LServer} = User,
      Peer, MsgID, MamID, TS, Msg) ->
    BarePeer = jid:encode(
		 jid:tolower(
		   jid:remove_resource(Peer))),
    XML = fxml:element_to_binary(xmpp:encode(Msg)),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("update inbox set "
		"timestamp=%(TS)d, "
		"mam_id=%(MamID)s, "
		"msg_id=%(MsgID)s, "
		"xml=%(XML)s, "
		"unread=unread+1 "
		"where username=%(LUser)s "
		"and peer=%(BarePeer)s "
		"and %(LServer)H")) of
	{updated, 0} ->
	    %% There's a little race at this point, but it's not the end of the
	    %% world if we run into it, so it's not worth the overhead of a
	    %% transaction.
	    case ejabberd_sql:sql_query(
		   LServer,
		   ?SQL_INSERT(
		      "inbox",
		      ["username=%(LUser)s",
		       "server_host=%(LServer)s",
		       "peer=%(BarePeer)s",
		       "timestamp=%(TS)d",
		       "mam_id=%(MamID)s",
		       "msg_id=%(MsgID)s",
		       "xml=%(XML)s",
		       "unread=1"])) of
		{updated, _} ->
		    ok;
		Err ->
		    ?ERROR_MSG("Cannot insert peer ~s into inbox of ~s: ~p",
			       [jid:encode(Peer), jid:encode(User), Err]),
		    {error, db_failure}
	    end;
	{updated, 1} ->
	    ok;
	Err ->
	    ?ERROR_MSG("Cannot update inbox for ~s in inbox of ~s: ~p",
		       [jid:encode(Peer), jid:encode(User), Err]),
	    {error, db_failure}
    end.

-spec maybe_reset_unread(jid(), jid(), binary())
      -> boolean() | {error, db_failure}.
maybe_reset_unread(#jid{luser = LUser, lserver = LServer} = User, Peer, ID) ->
    BarePeer = jid:encode(
		 jid:tolower(
		   jid:remove_resource(Peer))),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("update inbox set "
		"unread=0 "
		"where username=%(LUser)s "
		"and peer=%(BarePeer)s "
		"and %(LServer)H"
		"and (msg_id=%(ID)s or mam_id=%(ID)s)")) of
	{updated, 1} ->
	    true;
	{updated, 0} ->
	    false;
	Err ->
	    ?ERROR_MSG("Cannot reset counter for ~s in inbox of ~s: ~p",
		       [jid:encode(Peer), jid:encode(User), Err]),
	    {error, db_failure}
    end.

-spec reset_unread(jid(), jid()) -> ok | {error, db_failure}.
reset_unread(#jid{luser = LUser, lserver = LServer} = User, Peer) ->
    BarePeer = jid:encode(
		 jid:tolower(
		   jid:remove_resource(Peer))),
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("update inbox set "
		"unread=0 "
		"where username=%(LUser)s "
		"and peer=%(BarePeer)s "
		"and %(LServer)H")) of
	{updated, _} ->
	    ok;
	Err ->
	    ?ERROR_MSG("Cannot reset counter for ~s in inbox of ~s: ~p",
		       [jid:encode(Peer), jid:encode(User), Err]),
	    {error, db_failure}
    end.

%% TODO: Implement get() function.

-spec get_unread_total(jid())
      -> {unread, non_neg_integer} | {error, db_failure}.
get_unread_total(#jid{luser = LUser, lserver = LServer} = User) ->
    case ejabberd_sql:sql_query(
	   LServer,
	   ?SQL("select @(unread)d from inbox "
		"where username=%(LUser)s and %(LServer)H")) of
	{selected, Counts} ->
	    {unread, lists:foldl(fun({X}, Sum) -> Sum + X end, 0, Counts)};
	Err ->
	    ?ERROR_MSG("Cannot retrieve unread message count of ~s: ~p",
		       [jid:encode(User), Err]),
	    {error, db_failure}
    end.

-spec remove_user(binary(), binary()) -> any().
remove_user(LUser, LServer) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from inbox where username=%(LUser)s and %(LServer)H")).

-spec delete_old_inboxes(binary(), integer()) -> any().
delete_old_inboxes(LServer, TS) ->
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("delete from archive where timestamp < %(TS)d and %(LServer)H")).

%% TODO: Implement Mnesia export/1.
