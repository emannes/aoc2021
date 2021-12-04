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
  val test_input : string
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

  let test_input = {|
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
  |}

  let%expect_test _ =
    let input = parse_input (clean_input test_input) in
    print_s [%message (solve_a input : int)];
    let%bind () = [%expect {| ("solve_a input" 7) |}] in
    print_s [%message (solve_b input : int)];
    [%expect {| ("solve_b input" 5) |}]
  ;;
end

module Problem_2 : S = struct
  module Direction = struct
    module T = struct
      type t =
        | Forward
        | Down
        | Up
      [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  module Instruction = struct
    type t =
      { direction : Direction.t
      ; units : int
      }

    let of_string s =
      let direction, units = String.rsplit2_exn s ~on:' ' in
      { direction = Direction.of_string (String.capitalize direction)
      ; units = Int.of_string units
      }
    ;;
  end

  type t = Instruction.t list

  let parse_input = List.map ~f:Instruction.of_string

  let solve_a instructions =
    let final_depth =
      List.sum
        (module Int)
        instructions
        ~f:(fun { Instruction.direction; units } ->
          match direction with
          | Down -> units
          | Up -> -units
          | Forward -> 0)
    in
    let final_horizontal_position =
      List.sum
        (module Int)
        instructions
        ~f:(fun { direction; units } ->
          match direction with
          | Down | Up -> 0
          | Forward -> units)
    in
    final_depth * final_horizontal_position
  ;;

  module State = struct
    type t =
      { horizontal_position : int
      ; depth : int
      ; aim : int
      }

    let init = { horizontal_position = 0; depth = 0; aim = 0 }
  end

  let solve_b instructions =
    let { State.horizontal_position; depth; aim = _ } =
      List.fold
        instructions
        ~init:State.init
        ~f:(fun state { Instruction.direction; units } ->
          match direction with
          | Down -> { state with aim = state.aim + units }
          | Up -> { state with aim = state.aim - units }
          | Forward ->
            { state with
              horizontal_position = state.horizontal_position + units
            ; depth = state.depth + (state.aim * units)
            })
    in
    horizontal_position * depth
  ;;

  let test_input = {|
  forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2
  |}

  let%expect_test _ =
    let input = parse_input (clean_input test_input) in
    print_s [%message (solve_a input : int)];
    let%bind () = [%expect {| ("solve_a input" 150) |}] in
    print_s [%message (solve_b input : int)];
    [%expect {| ("solve_b input" 900) |}]
  ;;
end

module Problem_3 : S = struct
  type t = int list list

  let parse_input =
    List.map ~f:(fun s ->
        String.to_list s
        |> List.map ~f:(fun digit -> String.of_char digit |> Int.of_string))
  ;;

  let to_bit num_1s ~length ~kind =
    let majority_1s = num_1s >= length / 2 in
    match kind with
    | `gamma -> if majority_1s then "1" else "0"
    | `epsilon -> if majority_1s then "0" else "1"
  ;;

  let solve_a input =
    let length = List.length input in
    let rate (kind : [ `gamma | `epsilon ]) =
      "0b"
      ^ (List.transpose_exn input
        |> List.map ~f:(List.sum (module Int) ~f:Fn.id)
        |> List.map ~f:(to_bit ~length ~kind)
        |> String.concat)
      |> Int.of_string
    in
    rate `gamma * rate `epsilon
  ;;

  let filter_to input ~i ~(kind : [ `majority | `minority ]) =
    let length = List.length input in
    let num_1s = List.count input ~f:(fun row -> 1 = List.nth_exn row i) in
    let desired_digit =
      match kind with
      | `majority -> if num_1s >= (length + 1) / 2 then 1 else 0
      | `minority -> if num_1s >= (length + 1) / 2 then 0 else 1
    in
    List.filter input ~f:(fun row -> desired_digit = List.nth_exn row i)
  ;;

  let solve_b input =
    let row_length = List.hd_exn input |> List.length in
    let get_rating kind =
      Int.of_string
        ("0b"
        ^ (List.fold (List.init row_length ~f:Fn.id) ~init:input ~f:(fun input i ->
               if List.length input = 1 then input else filter_to input ~i ~kind)
          |> List.hd_exn
          |> List.map ~f:Int.to_string
          |> String.concat))
    in
    get_rating `majority * get_rating `minority
  ;;

  let test_input =
    {|
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|}
  ;;

  let%expect_test _ =
    let input = parse_input (clean_input test_input) in
    print_s [%message (solve_a input : int)];
    let%bind () = [%expect {| ("solve_a input" 198) |}] in
    print_s [%message (solve_b input : int)];
    [%expect {| ("solve_b input" 230) |}]
  ;;
end

let problems = [ (module Problem_1 : S); (module Problem_2 : S); (module Problem_3 : S) ]

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
