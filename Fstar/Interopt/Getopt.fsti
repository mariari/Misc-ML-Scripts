module Getopt

open FStar.ST
open FStar.All
open FStar.Pervasives

val noshort : Char.char

val nolong : string

noeq type opt_variant 'a =
  | ZeroArgs of (unit -> 'a)
  | OneArg of (string -> 'a) * string

type opt' 'a = Char.char * string * opt_variant 'a * string

type opt = opt' unit

type parse_cmdline_res =
  | Help
  | Error of string
  | Success

val parse_cmdline: list 'opt -> (string -> 'a) -> parse_cmdline_res
val parse_string: list 'opt -> (string -> 'a) -> string -> parse_cmdline_res
val cmdline: unit -> list string
