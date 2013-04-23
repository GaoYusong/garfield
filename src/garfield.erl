%%%-------------------------------------------------------------------
%%% @author GaoYusong <yusong.gao@gmail.com>
%%% @copyright (C) 2013, GaoYusong
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2013 by GaoYusong <yusong.gao@gmail.com>
%%%-------------------------------------------------------------------
-module(garfield).
-author("yusong.gao@gmail.com").

-export([recursive_started/1, del_dir/1,
	 cascade_value/2, cascade_value/3,
	 cascade_set/3, knapsack/2,
	 forbreak/3, merge_kvlist/1,
	 lists_head/2]).

%% @doc recursive start App, use dfs algorithm
-spec recursive_started(atom()) -> ok | {error, term()}.
recursive_started(App) ->
    do_recursive_started(App, [App]).

%% @doc merge kv list, [{1, a}, {1, b}, {2, c}, {2, c}] -> [{1, [a, b]}, {2, c}].
merge_kvlist(KVList) ->
    do_merge_kvlist(
      lists:usort(KVList), []).

%% @doc get head N elements of list L
lists_head(L, N) ->
  do_lists_head(L, 0, N, []).

%% @doc 
cascade_value(Keys, List) ->
    cascade_value(Keys, List, undefined).

%% @doc
cascade_value(Keys, List, Default) when is_list(Keys) ->
    lists:foldl(
      fun
	  (Key, {true, Last}) ->
	      case is_list(Last) of 
		  true ->
		      case proplists:get_value(Key, Last) of
			  undefined ->
			      {false, Default};
			  N ->
			      {true, N}
		      end;
		  false ->
		      {false, Default}
	      end;
	  (_, {false, Last}) ->
	      {false, Last}
      end, {true, List}, Keys).

%% @doc
cascade_set([], Value, _List) ->
    Value;
cascade_set([Key | Keys], Value, List) when is_list(List) ->
    case proplists:get_value(Key, List, cascade_set_undefined) of
        cascade_set_undefined ->
            List;
        NList ->
	    NValue = cascade_set(Keys, Value, NList),
	    [{Key, NValue} | proplists:delete(Key, List)]
    end;
cascade_set(_Keys, _Value, List) ->
    List.

%% @doc the implement of dp algorithm to solve knapsack problem
knapsack(Args, S) when is_list(Args) ->
    T = ets:new(dp, [set]),
    Res = dp(Args, length(Args), S, T),
    ets:delete(T),
    Res.

%% @doc forbreak like lists:foldl, except when you return {break, Ret}, it will return ret immediately
forbreak(Func, Base, Lists) when is_function(Func, 2) 
				 andalso is_list(Lists) ->
    forbreak_(Func, Base, Lists).

%% @doc del dir even the dir is not empty
del_dir(Path) ->
    case filelib:is_dir(Path) of
	true ->
	    {ok, Sub} = file:list_dir(Path),
	    case length(Sub) > 0 of
		true -> case lists:foldl(
			       fun (SubName, Sum) ->
				       case Sum of
					   ok -> 
					       case del_dir(filename:join([Path, SubName])) of
						   ok -> ok;
						   Err -> Err
					       end;
					   Err -> Err
				       end
			       end, ok, Sub)
			of
			    ok -> file:del_dir(Path);
			    Err -> Err
			end;
		false -> file:del_dir(Path)
	    end;
	false ->
	    case filelib:is_file(Path) of
		true -> 
		    file:delete(Path);
		false ->
		    {error, enoent}
	    end
    end.

%%%===================================================================
%%% Internal function
%%%===================================================================

do_recursive_started(App, AppPath) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, NextApp}} ->
            case lists:member(NextApp, AppPath) of
                true ->
                    {error, circular_dependency};
                false ->
                    case do_recursive_started(NextApp, [NextApp | AppPath]) of
                        ok ->
                            do_recursive_started(App, AppPath);
                        {error, _Reason} = Error ->
                            Error
                    end
            end;
        {error, _Reason} = Error ->
            Error
    end.

forbreak_(_Func, Result, []) -> 
    Result;
forbreak_(Func, Result, [H | T]) -> 
    case Func(H, Result) of
	{break, Ret} ->
	    Ret;
	Ret ->
	    forbreak_(Func, Ret, T)
    end.

dp(_, 0, _, _) ->
    0;
dp([{D, C} | R], N, S, T) ->
    case ets:lookup(T, {N, S}) of
	[] ->
	    Res = dp(R, N - 1, S, T),
	    Res2 = case S >= D of
		       true -> max(dp(R, N - 1, S - D, T) + C, Res);
		       _ -> Res
		   end,
	    ets:insert(T, {{N, S}, Res2}),
	    Res2;
	[{_Key, Val}] ->
	    Val
    end.

do_merge_kvlist([], Result) ->
    Result;
do_merge_kvlist([{Key, Val} | Rest], []) ->
    do_merge_kvlist(Rest, [{Key, [Val]}]); 
do_merge_kvlist([{Key, Val} | Rest], [{Key, Vals} | Result]) ->
    do_merge_kvlist(Rest, [{Key, [Val | Vals]} | Result]);
do_merge_kvlist([{Key, Val} | Rest], Result) ->
    do_merge_kvlist(Rest, [{Key, [Val]} | Result]);
do_merge_kvlist([_Head | Rest], Result) ->
    do_merge_kvlist(Rest, Result).

do_lists_head([], _, _, Result) ->
  lists:reverse(Result);
do_lists_head(_, _N, _N, Result) ->
  lists:reverse(Result);
do_lists_head([H | T], X, N, Result) ->
  do_lists_head(T, X + 1, N, [H | Result]).
