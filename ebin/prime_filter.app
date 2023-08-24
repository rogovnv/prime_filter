%%%-------------------------------------------------------------------
%%% @author mt
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 8. авг. 2023 16:46
%%%-------------------------------------------------------------------
{application, prime_filter, [
  {description, "Generator&Filter for primes"},
  {vsn, "1.0"},
  {registered, [top_sup]},
  {applications, [
    sasl,
    kernel,
    stdlib
  ]},
  {mod, {prime_filter, []}},
  {env, []}
]}.