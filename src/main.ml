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

module Problem_4 : S = struct
  module Victory = struct
    type t =
      { num_calls : int
      ; last_call : int
      ; sum_uncalled : int
      }
  end

  module Board = struct
    type t = int option list list

    let mark_call t call : t =
      List.map
        t
        ~f:
          (List.map ~f:(function
              | Some n when call = n -> None
              | Some n -> Some n
              | None -> None))
    ;;

    let sum_uncalled t =
      List.concat t |> List.filter_opt |> List.sum (module Int) ~f:Fn.id
    ;;

    let is_bingo t =
      let horizontal = List.exists t ~f:(List.for_all ~f:Option.is_none) in
      let vertical =
        List.exists (List.transpose_exn t) ~f:(List.for_all ~f:Option.is_none)
      in
      horizontal || vertical
    ;;
  end

  type t =
    { boards : Board.t list
    ; calls : int list
    }

  let parse_input input =
    let calls, boards =
      match input with
      | calls :: boards -> calls, boards
      | _ -> failwith "can't parse"
    in
    let calls = String.split ~on:',' calls |> List.map ~f:Int.of_string in
    let boards =
      List.map boards ~f:(fun line ->
          String.split line ~on:' '
          |> List.filter ~f:(fun s -> not (String.is_empty s))
          |> List.map ~f:(fun i -> Some (Int.of_string i)))
      |> List.groupi ~break:(fun i _ _ -> i mod 5 = 0)
    in
    { calls; boards }
  ;;

  let solve_a _ = failwith "not implemented"
  let solve_b _ = failwith "not implemented"

  let test_input =
    {| 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11  0
   8  2 23  4 24
  21  9 14 16  7
   6 10  3 18  5
   1 12 20 15 19
  
   3 15  0  2 22
   9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6
  
  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
   2  0 12  3  7
   |}
  ;;

  let%expect_test _ =
    let input = parse_input (clean_input test_input) in
    print_s [%message (solve_a input : int)];
    let%bind () = [%expect {| ("solve_a input" 198) |}] in
    print_s [%message (solve_b input : int)];
    [%expect {| ("solve_b input" 230) |}]
  ;;
end

module Problem_5 : S = struct
  module Point = struct
    module T = struct
      type t = int * int [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  module Line = struct
    type t = Point.t * Point.t [@@deriving sexp]

    let is_horizontal ((_, y1), (_, y2)) = y1 = y2
    let is_vertical ((x1, _), (x2, _)) = x1 = x2

    let range x1 x2 =
      let min = Int.min x1 x2 in
      let max = Int.max x1 x2 in
      List.range ~stride:1 ~start:`inclusive ~stop:`inclusive min max
    ;;

    let points_if_axis_aligned (((x1, y1), (x2, y2)) as t) =
      if is_horizontal t
      then List.map (range x1 x2) ~f:(fun x -> x, y1)
      else if is_vertical t
      then List.map (range y1 y2) ~f:(fun y -> x1, y)
      else (
        let to_step c1 c2 = Int.sign (c2 - c1) |> Sign.to_int in
        let stepx, stepy = to_step x1 x2, to_step y1 y2 in
        let length = Int.abs (x2 - x1) + 1 in
        List.init length ~f:(fun i -> x1 + (i * stepx), y1 + (i * stepy)))
    ;;
  end

  type t = Line.t list [@@deriving sexp]

  let parse_input =
    List.map ~f:(fun s ->
        String.split_on_chars s ~on:(String.to_list ", ->")
        |> List.filter ~f:(fun s -> not (String.is_empty s))
        |> List.map ~f:Int.of_string
        |> function
        | [ x1; y1; x2; y2 ] -> (x1, y1), (x2, y2)
        | _ -> failwith "can't parse")
  ;;

  let num_duplicated_points lines =
    let point_frequencies =
      List.concat_map lines ~f:Line.points_if_axis_aligned
      |> List.map ~f:(fun point -> point, ())
      |> Point.Map.of_alist_multi
      |> Map.map ~f:List.length
    in
    Map.filter point_frequencies ~f:(fun freq -> freq > 1) |> Map.length
  ;;

  let solve_a lines =
    List.filter lines ~f:(fun line -> Line.(is_vertical line || is_horizontal line))
    |> num_duplicated_points
  ;;

  let solve_b = num_duplicated_points

  let test_input =
    {|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|}
  ;;

  let%expect_test _ =
    let input = parse_input (clean_input test_input) in
    print_s [%message (solve_a input : int)];
    let%bind () = [%expect {| ("solve_a input" 5) |}] in
    print_s [%message (solve_b input : int)];
    [%expect {| ("solve_b input" 12) |}]
  ;;
end

let problems =
  [ (module Problem_1 : S)
  ; (module Problem_2 : S)
  ; (module Problem_3 : S) (* CR emannes: problem 4 *)
  ; (module Problem_3)
  ; (module Problem_5)
  ]
;;

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
