%%% Check conversations.im account expiry.
%%%
%%% UNPUBLISHED CODE.

-module(mod_check_expiry).
-author('holger@zedat.fu-berlin.de').
-compile([{parse_transform, ejabberd_sql_pt}]).
-behavior(gen_mod).

-export([start/2, stop/1, reload/3, mod_opt_type/1, depends/2]).
-export([c2s_auth_result/3]).

-include("ejabberd_sql_pt.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

-define(PAYMENT_REQUIRED_TEXT, <<"Please renew your subscription on "
				 "https://account.conversations.im/login/">>).

%% gen_mod callbacks.

-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _Opts) ->
    ejabberd_hooks:add(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 200).

-spec stop(binary()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(c2s_auth_result, Host, ?MODULE, c2s_auth_result, 200).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].
mod_opt_type(_Opt) ->
    [].

%% ejabberd_hooks callback.

-spec c2s_auth_result(ejabberd_c2s:state(), boolean(), binary())
      -> ejabberd_c2s:state() | {stop, ejabberd_c2s:state()}.
c2s_auth_result(#{lserver := <<"conversations.im">>} = State, true, User) ->
    LUser = jid:nodeprep(User),
    Result = ejabberd_sql:sql_query(
	       <<"conversations.im">>,
	       ?SQL("select @(unix_timestamp(expires))d "
		    "from users where username=%(LUser)s")),
    check_expiry(State, LUser, os:system_time(seconds), Result);
c2s_auth_result(#{lserver := LServer} = State, true, User) -> % Hosted domain.
    LUser = jid:nodeprep(User),
    Result = ejabberd_sql:sql_query(
	       LServer,
	       ?SQL("select @(unix_timestamp(expires))d "
		    "from backend.domain_account where domain=%(LServer)s")),
    check_expiry(State, LUser, os:system_time(seconds), Result);
c2s_auth_result(State, false, _User) -> % Authentication failed.
    State.

%% Internal functions.

-spec check_expiry(ejabberd_c2s:state(), binary(), integer(),
		   {selected, [{integer()}]} | {error, binary()})
      -> ejabberd_c2s:state() | {stop, ejabberd_c2s:state()}.
check_expiry(#{lserver := LServer} = State, LUser, Now,
	     {selected, [{Expiry}]}) when Now < Expiry ->
    ?DEBUG("Account ~s@~s has not expired", [LUser, LServer]),
    State;
check_expiry(#{lserver := LServer} = State, LUser, Now,
	     {selected, [{Expiry}]}) ->
    {D, {H, _, _}} = calendar:seconds_to_daystime(Now - Expiry),
    ?INFO_MSG("Account ~s@~s expired ~B day(s), ~B hour(s) ago",
	      [LUser, LServer, D, H]),
    disconnect(State, payment_required);
check_expiry(#{lserver := LServer} = State, LUser, _Now,
	     {error, Reason}) ->
    ?ERROR_MSG("Cannot get account expiry date for ~s@~s: ~s",
	       [LUser, LServer, Reason]),
    disconnect(State, db_failure).

-spec disconnect(ejabberd_c2s:state(), payment_required | db_failure)
      -> {stop, ejabberd_c2s:state()}.
disconnect(#{lang := Lang} = State, payment_required) ->
    SaslErr = #sasl_failure{reason = 'account-disabled',
			    text = xmpp:mk_text(?PAYMENT_REQUIRED_TEXT, Lang)},
    StreamErr = xmpp:serr_policy_violation(?PAYMENT_REQUIRED_TEXT, Lang),
    disconnect(State, SaslErr, StreamErr);
disconnect(#{lang := Lang} = State, db_failure) ->
    SaslErr = #sasl_failure{reason = 'temporary-auth-failure',
			    text = xmpp:mk_text(<<"Database failure">>, Lang)},
    StreamErr = xmpp:serr_resource_constraint(<<"Database failure">>, Lang),
    disconnect(State, SaslErr, StreamErr).

-spec disconnect(ejabberd_c2s:state(), sasl_failure(), stream_error())
      -> {stop, ejabberd_c2s:state()}.
disconnect(State, SaslErr, StreamErr) ->
    State1 = ejabberd_c2s:send(State, SaslErr),
    State2 = ejabberd_c2s:send(State1, StreamErr),
    {stop, State2}.
