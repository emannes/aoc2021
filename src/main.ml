open! Core
open Async

let clean_input input =
  String.split_lines input
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun line -> not (String.is_empty line))
;;

module Subpart = struct
  type t =
    | A
    | B
  [@@deriving sexp]
end

module type S = sig
  type t

  val parse_input : string list -> t
  val solve_a : t -> int
  val solve_b : t -> int
end

module Problem_1 : S = struct
  type t = int array

  let parse_input = Array.of_list_map ~f:Int.of_string

  let num_increases t ~gap =
    Array.counti t ~f:(fun i ith_depth ->
        if i + gap >= Array.length t then false else Array.get t (i + gap) > ith_depth)
  ;;

  let solve_a = num_increases ~gap:1
  let solve_b = num_increases ~gap:3

  let%expect_test _ =
    let input = {|
  199
200
208
210
200
207
240
269
260
263
  |} in
    let input = parse_input (clean_input input) in
    print_s [%message (solve_a input : int)];
    let%bind () = [%expect {| ("solve_a input" 7) |}] in
    print_s [%message (solve_b input : int)];
    [%expect {| ("solve_b input" 5) |}]
  ;;
end

let problems = [ (module Problem_1 : S) ]

let command =
  Command.async
    ~summary:"Advent of Code 2021 solutions"
    (let open Command.Let_syntax in
    let%map_open () = return ()
    and problem = anon ("PROBLEM" %: int)
    and part = anon ("PART" %: sexp_conv Subpart.t_of_sexp)
    and input_file = flag "-input" ~doc:"FILE input file" (required Filename.arg_type) in
    fun () ->
      let open Async in
      let (module P : S) = List.nth_exn problems (problem - 1) in
      let%bind input =
        Reader.file_contents input_file >>| clean_input >>| P.parse_input
      in
      let answer =
        match part with
        | A -> P.solve_a input
        | B -> P.solve_b input
      in
      print_int answer;
      Deferred.unit)
;;
