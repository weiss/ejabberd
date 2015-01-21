%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via P1DB
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

-module(ejabberd_auth_p1db).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2, init_db/1,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0, export/1,
	 enc_key/1, dec_key/1,
	 import/2, plain_password_required/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    init_db(Host),
    ok.

init_db(Host) ->
    Group = ejabberd_config:get_option(
	      {p1db_group, Host}, fun(G) when is_atom(G) -> G end),
    p1db:open_table(passwd,
                    [{group, Group},
                     {schema, [{keys, [server, user]},
                               {vals, [password]},
                               {enc_key, fun ?MODULE:enc_key/1},
                               {dec_key, fun ?MODULE:dec_key/1}]}]).

plain_password_required() -> false.

store_type() -> plain.

us2key(LUser, LServer) ->
    <<LServer/binary, 0, LUser/binary>>.

key2us(Key) ->
    [LServer, LUser] = binary:split(Key, <<0>>, [global]),
    {LUser, LServer}.

server_prefix(Server) ->
    LServer = jlib:nameprep(Server),
    <<LServer/binary, 0>>.

check_password(_User, _Server, <<>>) ->
    false;
check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if LUser /= error, LServer /= error ->
            US = us2key(LUser, LServer),
            case p1db:get(passwd, US) of
                {ok, Passwd, _VClock} ->
                    Passwd == Password;
                {error, _} ->
                    false
            end;
       true ->
            false
    end.

check_password(User, Server, Password, Digest, DigestGen) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if LUser /= error, LServer /= error ->
            US = us2key(LUser, LServer),
            case p1db:get(passwd, US) of
                {ok, Passwd, _VClock} ->
                    DigRes = if Digest /= <<"">> ->
                                     Digest == DigestGen(Passwd);
                                true -> false
                             end,
                    if DigRes -> true;
                       true -> (Passwd == Password) and (Password /= <<"">>)
                    end;
                {error, _} -> false
            end;
       true ->
            false
    end.

-spec set_password(binary(), binary(), binary()) ->
                          ok | {error, invalid_jid} | p1db:error().
set_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
       true ->
            US = us2key(LUser, LServer),
            p1db:insert(passwd, US, Password)
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
try_register(User, Server, PasswordList) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Password = iolist_to_binary(PasswordList),
    if (LUser == error) or (LServer == error) ->
            {error, invalid_jid};
       true ->
            US = us2key(LUser, LServer),
            case p1db:get(passwd, US) of
                {ok, _, _} ->
                    {atomic, exists};
                {error, notfound} ->
                    case p1db:insert(passwd, US, Password) of
                        ok ->
                            {atomic, ok};
                        Err ->
                            {aborted, Err}
                    end;
                {error, _} = Err ->
                    {aborted, Err}
            end
    end.

dirty_get_registered_users() ->
    case p1db:get(passwd) of
        {ok, L} ->
            [key2us(USKey) || {USKey, _Passwd, _VClock} <- L];
        {error, _} ->
            []
    end.

get_vh_registered_users(Server) ->
    ServerPrefix = server_prefix(Server),
    case p1db:get_by_prefix(passwd, ServerPrefix) of
        {ok, L} ->
            [key2us(USKey) || {USKey, _Passwd, _VClock} <- L];
        {error, _} ->
            []
    end.

get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

get_vh_registered_users_number(Server) ->
    ServerPrefix = server_prefix(Server),
    case p1db:count_by_prefix(passwd, ServerPrefix) of
        {ok, N} -> N;
        _Err -> 0
    end.

get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if LUser /= error, LServer /= error ->
            US = us2key(LUser, LServer),
            case p1db:get(passwd, US) of
                {ok, Passwd, _VClock} -> Passwd;
                {error, _} -> false
            end;
       true ->
            false
    end.

get_password_s(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if LUser /= error, LServer /= error ->
            US = us2key(LUser, LServer),
            case p1db:get(passwd, US) of
                {ok, Passwd, _VClock} -> Passwd;
                {error, _} -> <<"">>
            end;
       true ->
            <<"">>
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if LUser /= error, LServer /= error ->
            US = us2key(LUser, LServer),
            case p1db:get(passwd, US) of
                {ok, _Passwd, _VClock} -> true;
                {error, notfound} -> false;
                {error, _} = Err -> Err
            end;
       true ->
            {error, invalid_jid}
    end.

%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if LUser /= error, LServer /= error ->
            US = us2key(LUser, LServer),
            p1db:delete(passwd, US),
            ok;
       true ->
            ok
    end.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    if LUser /= error, LServer /= error ->
            US = us2key(LUser, LServer),
            case p1db:get(passwd, US) of
                {ok, Password, _VClock} ->
                    p1db:delete(passwd, US),
                    ok;
                {ok, _DifferentPassword, _VClock} ->
                    not_allowed;
                {error, notfound} ->
                    not_exists;
                {error, _} ->
                    bad_request
            end;
       true ->
            bad_request
    end.

%% P1DB/SQL Schema
enc_key([Server]) ->
    <<Server/binary>>;
enc_key([Server, User]) ->
    <<Server/binary, 0, User/binary>>.

dec_key(Key) ->
    Len = str:chr(Key, 0) - 1,
    <<Server:Len/binary, 0, User/binary>> = Key,
    [Server, User].

%% Export/Import
export(_Server) ->
    [{passwd,
      fun(Host, {USKey, Password}) ->
              {LUser, LServer} = key2us(USKey),
              if LServer == Host ->
                      Username = ejabberd_odbc:escape(LUser),
                      Pass = ejabberd_odbc:escape(Password),
                      [[<<"delete from users where username='">>,
                        Username, <<"';">>],
                       [<<"insert into users(username, password) "
                          "values ('">>, Username, <<"', '">>,
                        Pass, <<"');">>]];
                 true ->
                      []
              end
      end}].

import(LServer, [LUser, Password, _TimeStamp]) ->
    US = us2key(LUser, LServer),
    p1db:async_insert(passwd, US, Password).
