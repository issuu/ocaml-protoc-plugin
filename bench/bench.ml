open Base
open Stdio

module type Protobuf = sig
  type t
  val name : string
  val encode : t -> string
  val decode : string -> t
end
let _ = Random.init 0

module Protoc_mod : Protobuf = struct
  type t = Protoc.Bench.btree
  let name = "Protoc"
  let encode t =
    let encoder = Pbrt.Encoder.create () in
    Protoc.Bench.encode_pb_btree t encoder;
    Pbrt.Encoder.to_string encoder

  let decode data =
    let decoder = Pbrt.Decoder.of_string data in
    Protoc.Bench.decode_pb_btree decoder
end

module Plugin_mod : Protobuf with type t = Plugin.Bench.Bench.Btree.t = struct
  type t = Plugin.Bench.Bench.Btree.t
  let name = "Plugin"
  let encode t =
    let writer = Plugin.Bench.Bench.Btree.to_proto t in
    Ocaml_protoc_plugin.Writer.contents writer

  let decode data =
    let reader = Ocaml_protoc_plugin.Reader.create data in
    Plugin.Bench.Bench.Btree.from_proto reader |> Ocaml_protoc_plugin.Result.get ~msg:"Unable to decode"
end

let create_test_data ~depth () =
  let module Btree = Plugin.Bench.Bench.Btree in
  let module Data = Plugin.Bench.Bench.Data in
  let module Enum = Plugin.Bench.Bench.Enum in
  let optional ~f () =
    match (Random.int 4 = 0) with
    | true -> None
    | false -> Some (f ())
  in
  let random_string () =
    String.init (Random.int 20) ~f:(fun _ -> Random.char ())
  in

  let random_list ?(len=100) ~f () =
    List.init (Random.int len) ~f:(fun _ -> f ())
  in

  let create_data () =

    let random_enum () =
      match Random.int 5 with
      | 0 -> Enum.EA
      | 1 -> Enum.EB
      | 2 -> Enum.EC
      | 3 -> Enum.ED
      | 4 -> Enum.EE
      | _ -> failwith "Impossible value"
    in
    let s1 = optional ~f:random_string () in
    let n1 = optional ~f:(random_list ~f:(fun () -> Random.int 1_000)) () in
    let n2 = optional ~f:(random_list ~f:(fun () -> Random.int 1_000)) () in
    let d1 = optional ~f:(random_list ~f:(fun () -> Random.float 1_000.)) () in
    let n3 = optional ~f:(fun () -> Random.int 1_000) () in
    let b1 = optional ~f:Random.bool () in
    let _e = optional ~f:(random_list ~f:random_enum) () in

    Data.make ?s1 ?n1 ?n2 ?d1 ?n3 ?b1 (* ?e *) ()
  in

  let rec create_btree n () =
    match n with
    | 0 -> None
    | n ->
      let data = random_list ~f:create_data () in
      let children =
        random_list ~len:8 ~f:(create_btree (n - 1)) () |> List.filter_opt
      in
      Btree.make ~children ~data () |> Option.some
  in
  create_btree depth ()

let make_test (module P : Protobuf) data_str =
  let data = P.decode data_str in
  let open Bechamel in
  let test_decode = Test.make ~name:"decode" (Staged.stage @@ fun () -> P.decode data_str) in
  let test_encode = Test.make ~name:"encode" (Staged.stage @@ fun () -> P.encode data) in
  Test.make_grouped ~name:P.name [test_decode; test_encode]

let make_tests data_str =
  [ make_test (module Protoc_mod) data_str; make_test (module Plugin_mod) data_str ]
  |> Bechamel.Test.make_grouped ~name:"Protobuf"


let benchmark tests =
  let open Bechamel in
  let instances = Bechamel_perf.Instance.[ cpu_clock ] in
  let cfg = Benchmark.cfg ~limit:10000 ~stabilize:true ~compaction:true
              ~quota:(Time.second 2.5) () in
  Benchmark.all cfg instances tests

let analyze results =
  let open Bechamel in
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true
    ~predictors:[| Measure.run |] in
  let results = Analyze.all ols Bechamel_perf.Instance.cpu_clock results in
  Analyze.merge ols [ Bechamel_perf.Instance.cpu_clock ] [ results ]

let print_bench_results results =
  let open Bechamel in
  let () = Bechamel_notty.Unit.add
             Bechamel_perf.Instance.cpu_clock
             (Measure.unit Bechamel_perf.Instance.cpu_clock)
  in

  let img (window, results) =
    Bechamel_notty.Multiple.image_of_ols_results ~rect:window
      ~predictor:Measure.run results
  in

  let open Notty_unix in

  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w= 80; h= 1; } in
  img (window, results) |> eol |> output_image

let _ =
  let data = create_test_data ~depth:4 () in
  let data = Option.value_exn data in
  let proto_str = Plugin_mod.encode data in
  let _data = Plugin_mod.decode proto_str in
  let data_protoc = Protoc_mod.decode proto_str in
  let data_str' = Protoc_mod.encode data_protoc in
  let data' = Plugin_mod.decode data_str' in
  let data_str' = Plugin_mod.encode data' in
  printf "Data length: %d / %d %b\n%!" (String.length proto_str) (String.length data_str') (String.equal proto_str data_str');
  let module Gc = Stdlib.Gc in
  Gc.full_major ();
  let control = Gc.get () in
  Gc.set { control with minor_heap_size = 1024*1024*10; space_overhead=5 };

  make_tests proto_str
  |> benchmark
  |> analyze
  |> print_bench_results
