%% ------------------------------------------------------------------------
%% Copyright (c) 2014, Kook Maeng <kook.maeng@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%% ------------------------------------------------------------------------

-module(tellet_objects).

-export([
		 objects/3,
		 object/3,
		 value/2,
		 value/3,
		 delete/2,
		 delete_list/2,
		 append/2,
		 replace/3,
		 replace_value/3,
		 update/2,
		 is_all_defined/2,
		 is_any_defined/2
		]).

%% Public functions

objects(KeyName, KeyValue, {Objects}) ->
	objects(KeyName, KeyValue, Objects);
objects(KeyName, KeyValue, Objects) when is_list(Objects) ->
	F = fun({Obj}) ->
		proplists:get_value(KeyName, Obj, []) =:= KeyValue
	end,
	case lists:filter(F, Objects) of
		[] -> [];
		ObjectsFound -> ObjectsFound
	end.

object(KeyName, KeyValue, Objects) ->
	case objects(KeyName, KeyValue, Objects) of
		[] -> [];
		[H | _] -> H
	end.

value(Key, {Props}, Default) ->
	value(Key, Props, Default);
value(Key, Props, Default) when is_list(Props) ->
	proplists:get_value(Key, Props, Default).

value(Key, {Props}) ->
	value(Key, Props);
value(Key, Props) when is_list(Props) ->
	value(Key, Props, undefined).


delete(Key, {Props}) when is_binary(Key) ->
	{delete(Key, Props)};
delete(Key, Props) when is_binary(Key), is_list(Props) ->
	%lists:keydelete(Key, 1, Props);
	proplists:delete(Key, Props);
delete(Obj, {Objects}) ->
	delete(Obj, Objects);
delete(Obj, Objects) when is_list(Objects) ->
	lists:delete(Obj, Objects).

delete_list([], Output) -> Output;
delete_list([H | T], Props) when is_binary(H) ->
	%Props1 = lists:keydelete(H, 1, Props),
	Props1 = proplists:delete(H, Props),
	delete_list(T, Props1).


append(Obj, {Objects}) ->
	{append(Obj, Objects)};
append(Obj, Objects) when is_tuple(Obj), is_list(Objects) ->
	[Obj | Objects].

replace(ObjFrom, ObjTo, {Objects}) ->
	{replace(ObjFrom, ObjTo, Objects)};
replace(ObjFrom, ObjTo, Objects) when is_list(Objects) ->
	Objects1 = lists:delete(ObjFrom, Objects),
	[ObjTo | Objects1].


replace_value(Key, NewVal, {Props}) ->
	{replace_value(Key, NewVal, Props)};
replace_value(Key, NewVal, Props) when is_list(Props) ->
	%Props1 = lists:keydelete(Key, 1, Props),
	Props1 = proplists:delete(Key, Props),
	[{Key, NewVal} | Props1].

%% @todo enhance performance
update({OldProps}, {NewProps}) ->
	{update(OldProps, NewProps)};
update(OldProps, NewProps) when is_list(OldProps), is_list(NewProps) ->
	lists:foldl(
	  fun({N, _}, OldProps1) ->
			  %lists:keydelete(N, 1, OldProps1)
			  proplists:delete(N, OldProps1)
	  end, OldProps, NewProps
	 ) ++ NewProps.


is_all_defined(_, <<>>) -> false;
is_all_defined(_, []) -> false;
is_all_defined([], _) -> true;
is_all_defined([H | T], Props) when is_list(Props) ->
	%case lists:keymember(H, 1, Props) of
	case proplists:is_defined(H, Props) of
		true -> is_all_defined(T, Props);
		false -> false 
	end;
is_all_defined(List, {Props}) ->
	is_all_defined(List, Props).


is_any_defined(_, []) -> false;
is_any_defined([], _) -> false;
is_any_defined([H | T], Props) when is_list(Props) ->
	%case lists:keymember(H, 1, Props) of
	case proplists:is_defined(H, Props) of
		true -> true;
		false -> is_any_defined(T, Props)
	end;
is_any_defined(List, {Props}) ->
	is_any_defined(List, Props).

% Private functions




%% Tests

-ifdef(TEST_).
-include_lib("eunit/include/eunit.hrl").


-endif.


