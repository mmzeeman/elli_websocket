%%
%% Binary string helper
%% 

-module(elli_bstr).

-export([to_lower/1,
    is_equal_ci/2,
    lchr/1]).

to_lower(Bin) when is_binary(Bin) ->
        << <<(lchr(C))>> || <<C>> <= Bin >>.

%% @doc Compare two binary values, return true iff they are equal by a caseless compare.
is_equal_ci(Bin, Bin) ->
        % Quick match with an Erlang pattern match
        true;
is_equal_ci(Bin1, Bin2) when is_binary(Bin1) andalso is_binary(Bin2)
                andalso size(Bin1) =:= size(Bin2) ->
        % Both binaries are the same length, do a good check
        equal_ci(Bin1, Bin2);
is_equal_ci(_, _) ->
        false.

equal_ci(<<>>, <<>>) ->
        true;
equal_ci(<<C, Rest1/binary>>, <<C, Rest2/binary>>) ->
        equal_ci(Rest1, Rest2);
equal_ci(<<C1, Rest1/binary>>, <<C2, Rest2/binary>>) ->
        case lchr(C1) =:= lchr(C2) of
                true ->
                        equal_ci(Rest1, Rest2);
                false ->
                        false
        end.

% @doc convert character to lowercase.
lchr($A) -> $a;
lchr($B) -> $b;
lchr($C) -> $c;
lchr($D) -> $d;
lchr($E) -> $e;
lchr($F) -> $f;
lchr($G) -> $g;
lchr($H) -> $h;
lchr($I) -> $i;
lchr($J) -> $j;
lchr($K) -> $k;
lchr($L) -> $l;
lchr($M) -> $m;
lchr($N) -> $n;
lchr($O) -> $o;
lchr($P) -> $p;
lchr($Q) -> $q;
lchr($R) -> $r;
lchr($S) -> $s;
lchr($T) -> $t;
lchr($U) -> $u;
lchr($V) -> $v;
lchr($W) -> $w;
lchr($X) -> $x;
lchr($Y) -> $y;
lchr($Z) -> $z;
lchr(Chr) -> Chr.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

case_insensitive_equal_test() ->
        ?assertEqual(true, is_equal_ci(<<>>, <<>>)),
        ?assertEqual(true, is_equal_ci(<<"abc">>, <<"abc">>)),
        ?assertEqual(true, is_equal_ci(<<"123">>, <<"123">>)),

        ?assertEqual(false, is_equal_ci(<<"abcd">>, <<"abc">>)),
        ?assertEqual(false, is_equal_ci(<<"1234">>, <<"123">>)),

        ?assertEqual(true, is_equal_ci(<<"aBc">>, <<"abc">>)),
        ?assertEqual(true, is_equal_ci(<<"123AB">>, <<"123ab">>)),

        ok.

%% Test if to_lower works.
ascii_to_lower_test() ->
        ?assertEqual(<<>>, to_lower(<<>>)),
        ?assertEqual(<<"abc">>, to_lower(<<"abc">>)),
        ?assertEqual(<<"abc">>, to_lower(<<"ABC">>)),
        ?assertEqual(<<"1234567890abcdefghijklmnopqrstuvwxyz!@#$%^&*()">>,
                to_lower(<<"1234567890abcdefghijklmnopqrstuvwxyz!@#$%^&*()">>)),
        ?assertEqual(<<"1234567890abcdefghijklmnopqrstuvwxyz!@#$%^&*()">>,
                to_lower(<<"1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()">>)),
        ok.

-endif.

