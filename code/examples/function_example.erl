-module(function_example).
-export([f/1]).

f(2) -> "two";
f(1) -> "one";
f(0) -> "zero";
f(3) -> "three";
f(_) -> "unknown number".
