%%% @doc This module generates a set of uniform points around an ellipse.
%%% @end
-module(ellipint).
-export([ellipint/1, 
         ellipint/3, 

         arc_length/4,
         arc_length_to_angle/6, 

         circumferenceE/2,
         circumference/2, 

         eccentricity/2, 

         point_generator/3,
         cached_point_generator/3]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc Complete elliptic integral of the second kind.
%% http://www.exstrom.com/math/elliptic/ellipint.html
%% The length of the arc from 0 to Pi/2 of the ellipse with the eccentricity K.
ellipint(K) ->
    math:pi() / 2 * (1 - K*K / 4 * g(K)).

g(X) ->
    P = fun(N) -> math:pow(X, N) end,
    1 + P(2)    * 3     / 16 
      + P(4)    * 5     / 64 
      + P(6)    * 175   / 4096
      + P(8)    * 881   / 32768
      + P(10)   * 9647  / 524288
      + P(12)   * 27885 / 2097152
      + P(14)   * 669451/ 67108864.


%% @doc This function calculates the focal distance (c).
%% http://mysite.du.edu/~jcalvert/math/ellarc.htm
%% A is a major axis, B is a minor axis.
focal_distance(A, B) when A >= B ->
    math:sqrt(A*A - B*B).


%% @doc e = c/a, and 0 ≤ e ≤ 1.
%% http://en.wikipedia.org/wiki/Eccentricity_%28mathematics%29
%% A is a major axis, B is a minor axis.
eccentricity(A, B) ->
    focal_distance(A, B) / A.


%% @doc The total arc length (perimeter) of the ellipse (C).
%% A is a major axis, B is a minor axis.
circumference(A, B) ->
    E = eccentricity(A, B),
    4 * A * ellipint(E).

circumferenceE(E, A) ->
    4 * A * ellipint(E).


%% The length of the arc of the angle T (in rad) of the ellipse with the eccentricity K.
%% 1.57 for a circle of radius 1.
%%
%% 0 =< T <= Pi/2 (right-top quater).
ellipint(T, K, N) ->
    X = 2*N + 1,
    X*T - 2 / X * sigma_m(math:tan(T), K, 1, N, 0).


%% N is how many `sigma_m' cycles to repeat.
%% If K = 0, returns T.
%% A is a radius for circle.
arc_length(T, K, A, N) ->
%   io:format("T: ~p, K: ~p, A: ~p, N: ~p~n", [T, K, A, N]),
    HalfPi = math:pi() / 2,
    %% From 0. How many quaters are skipped.
    QuarterCount = trunc(T / HalfPi),
    %% From 0: 0, 1, 2, 3.
    QuarterNum = QuarterCount rem 4,
    AngleRem = T - QuarterCount * HalfPi,
    AngleCalc = case QuarterNum rem 2 of
            0 -> HalfPi - AngleRem;
            1 -> AngleRem
        end,
    true = 0 =< AngleRem,
    true = 0 =< AngleCalc,
    ArcLen = ellipint(AngleCalc, K, N),
    QuarterLen = ellipint(K),
    ArcLenCalc = case QuarterNum rem 2 of
            0 -> QuarterLen - ArcLen;
            1 -> ArcLen
        end,
    (QuarterNum * QuarterLen + ArcLenCalc) * A.


%% A = 0, X = 0.
%% A = Pi / 2, X = Pi / 2 when R = 1.
arc_length_to_angle(X, K, A, N, StepCount, Delta) ->
    F = fun(Par) -> arc_length(Par, K, A, N) end,
%   io:format("~4s ~10s ~10s ~10s ~10s ~10s~n", [ "StepCount", "From", "To", 
%                                                 "Par", "ExpValue", "Value"]),
    %% Is this arc length simple?
    C = circumferenceE(K, A),
%   [io:format(user, "~p ~p ~p~n", [X, C, simple_arc_length(X, C)]) || X > C],
    true = X =< C,
    %% Change angle (Par) from 0 to 2Pi.
    %% Find such Par, that X =:= arc_length(Par, ...)
    find_parameter(F, 0, math:pi() * 2, X, StepCount, Delta). 



%% @doc Make a minumum X, that represents the same point on the circle.
%% X is an arc's length.
%% C is a perimeter (circumference).
simple_arc_length(X, C) ->
    %% Float remain.
    FullWiseCount = trunc(X / C),
    X - FullWiseCount * C.


%% @doc Same as simple_arc_length/3, different parameters.
%% A is a major axis, B is a minor axis.
simple_arc_length(X, A, B) ->
    C = circumference(A, B),
    simple_arc_length(X, C).


%% @doc Search such value of parameter of F, that it is near this function 
%% returns `ExpectedValue'.
%% Par = [From .. To]
%%
%% It is a binary search.
find_parameter(F, From, To, ExpectedValue, StepCount, Delta) ->
    Par = (To - From) / 2 + From,
    Value = F(Par),
%   io:format("~4B ~10f ~10f ~10f ~10f ~10f~n", [ StepCount, float(From), float(To), 
%                                            float(Par),
%                                            float(ExpectedValue), float(Value) ]),
    IsInDelta = compare(Value, ExpectedValue, Delta),
    if 
      %% It is accurate enough or it was runned too long.
        IsInDelta; StepCount =:= 0 ->
        Par;
      Value > ExpectedValue ->
        find_parameter(F, From, Par, ExpectedValue, StepCount - 1, Delta);
      Value < ExpectedValue ->
        find_parameter(F, Par, To, ExpectedValue, StepCount - 1, Delta);
      true ->
        Par
    end.
    

%% Sum.
%% A is an accumulator.
%% TT is tan(T).
%%
%% Change M from 1 to K.
%% A part of the formula.
sigma_m(TT, K, M, N, A) when M =< N ->
    ThetaM = theta_m(M, N),
    RhoM = rho_m(K, ThetaM),
    R = math:pow(math:tan(ThetaM), 2) * math:atan(RhoM * TT) / RhoM,
    sigma_m(TT, K, M + 1, N, R + A);

sigma_m(_TT, _K, _M, _N, A) ->
    A.
    

%% A part of the formula.
theta_m(M, N) ->
    X = 2*N + 1,
    M * math:pi() / X.


%% A part of the formula.
rho_m(K, ThetaM) ->
    X = 1 - math:pow(K * math:cos(ThetaM), 2),
    math:sqrt(X).


%% @doc Generates N random points for an ellipse inside the ellipse,
%% parametered by (A cos t, B sin t).
point_generator(A, B, N) ->
    TotalArcLen = circumference(A, B),
    ArcLen = TotalArcLen / N,
    fun(X) when 1 =< X, X =< N ->
            P = (X - 1) * ArcLen,
            K =  eccentricity(A, B),
            Angle = arc_length_to_angle(P, K, A, 100, 100, 0.0001),
            {A*math:cos(Angle), B*math:sin(Angle)}
        end.


cached_point_generator(A, B, N) ->
    F = point_generator(A, B, N),
    T = list_to_tuple(lists:map(F, lists:seq(1, N))),
    fun(X) -> element(X, T) end.



-ifdef(TEST).

circumference_test_() ->
    [{"When a circle's diameter is 1 unit, its circumference is Pi units.",
      ?_assert(compare_or_error(circumference(0.5, 0.5), math:pi()))}
    , ?_assert(compare_or_error(circumference(2, 1), 9.688448220560909))
    ].


arc_length_test_() ->
    [{"Circle of radius 1, 0-180 degrees.",
      ?_assert(compare_or_error(arc_length(math:pi(), 0, 1, 100), math:pi()))}
    ,{"Circle of radius 2, 0-90 degrees.",
      ?_assert(compare_or_error(arc_length(math:pi() / 2, 0, 2, 100), math:pi()))}
    ].

%% The length of the arc from 0 to Pi/2 is two times shorter, that the arc
%% from 0 to Pi.
prop_arc_length() ->
    ?FORALL({X1, X2},
            {non_neg_float(), non_neg_float()},
        begin
            {B, A} = sort(X1, X2),
            true = A >= B,
            E = eccentricity(A, B),
            AL1 = arc_length(math:pi() / 2, E, A, 100),
            AL2 = arc_length(math:pi(), E, A, 100),
            %% Delta is AL1 / 50.
            equals(true, compare_or_error(AL1, AL2 / 2, AL1 / 50))
        end).


prop_circumference_point_position_to_angle() ->
    ?FORALL({{r1, R1}, {r2, R2}, {phi, Phi}},
            {{r1, non_neg_float()}, {r2, non_neg_float()}, {phi, non_neg_float()}},
        begin
            {B, A} = sort(R1, R2),
            true = A >= B,
            E = eccentricity(A, B),
            X = arc_length(Phi, E, A, 100),
            SX = simple_arc_length(X, A, B),
            Phi2 = arc_length_to_angle(X, E, A, 100, 100, 0),
            X2 = arc_length(Phi2, E, A, 100),
            SX2 = simple_arc_length(X2, A, B),
            equals(true, compare_or_error(SX, SX2))
        end).


run_property_testing_test() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{constraint_tries, 500}]),
    erlang:group_leader(EunitLeader, self()),
    ?assertEqual([], Res).



prop_find_parameter() ->
    ?FORALL({List, {ValueBorder1, ValueBorder2}},
            {[float(), float(), float()], {float(), float()}},
        begin
            [From, Par, To] = lists:sort(List),
            {ValueFrom, ValueTo} = sort(ValueBorder1, ValueBorder2),
            true = ValueFrom =< ValueTo,
            true = From =< To,
            ValRange = ValueTo - ValueFrom,
            ParRange = To - From,
            true = 0 =< ValRange,
            true = 0 =< ParRange,
            %% Linear function.
            F = fun(X) when From =< X, X =< To ->
                ParPos = X - From, 
                true = 0 =< ParPos,
                true = ParPos =< ParRange,
                %% ValPos      ParPos
                %% -------- =  ------
                %% ValRange    ParRange
                ValPos = ValRange * ParPos / ParRange,
                ValPos + ValueFrom
                end,
            ExpectedValue = F(Par),
            true = F(From) =< F(To),
            true = ExpectedValue =< ValueTo,
            true = ExpectedValue >= ValueFrom,
            FoundedPar = find_parameter(F, From, To, ExpectedValue, 1000, 0),
%           io:format(user, "~nPar: ~p FounedPar: ~p~n", [Par, FoundedPar]),
            equals(true, compare_or_error(FoundedPar, Par))
        end).


sort(X, Y) when X =< Y -> {X, Y};
sort(X, Y) -> {Y, X}.


compare_or_error(X, Y) -> compare_or_error(X, Y, 0.01).
compare_or_error(X, Y, D) -> 
    case compare(X, Y, D) of
        true -> true;
        false ->
            io:format(user, "Error: ~p, ~p, ~p~n", [X, Y, D]),
            {X, Y, D}
    end.

prop_compare() ->
     ?FORALL({X, D},
             {float(), float()},
             equals(true, compare_or_error(X, X+D/2, abs(D)))).


diff_test_() ->
    [?_assertEqual(diff(0, 1), 1)
    ,?_assertEqual(diff(-1, 1), 2)
    ,?_assertEqual(diff(-1, -2), 1)
    ,?_assertEqual(diff(3, 2), 1)
    ,?_assertEqual(diff(2, 3), 1)
    ].


-endif.

compare(X, Y, Delta) ->
    diff(X, Y) =< Delta.


diff(X, Y) ->
    abs(X - Y).
