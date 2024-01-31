[@@@ocaml.warning "-26"]
open Base
open Stdio

let meassure = Bechamel_perf.Instance.cpu_clock

[@@@ocaml.warning "-32"]
module type Protoc_impl = sig
  type m
  val encode_pb_m: m -> Pbrt.Encoder.t -> unit
  val decode_pb_m: Pbrt.Decoder.t -> m
end

module type Plugin_impl = sig
  module M : sig
    type t
    val name' : unit -> string
    val show: t -> string
    val equal: t -> t -> bool
    val to_proto: t -> Ocaml_protoc_plugin.Writer.t
    val to_proto': Ocaml_protoc_plugin.Writer.t -> t -> Ocaml_protoc_plugin.Writer.t
    val from_proto_exn: Ocaml_protoc_plugin.Reader.t -> t
  end
end

let make_tests (type v) (module Protoc: Protoc_impl) (module Plugin: Plugin_impl with type M.t = v) v_plugin =

  (* Verify *)
  let verify_identity ~mode data =
    let writer = Plugin.M.to_proto' (Ocaml_protoc_plugin.Writer.init ~mode ()) data in
    let data' = Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create (Ocaml_protoc_plugin.Writer.contents writer)) in
    let () = match Plugin.M.equal data data' with
      | true -> ()
      | false ->
        eprintf "Orig: %s\n" (Plugin.M.show data);
        eprintf "New: %s\n" (Plugin.M.show data');
        failwith "Data not the same"
    in
    Ocaml_protoc_plugin.Writer.contents writer |> String.length,
    Ocaml_protoc_plugin.Writer.unused_space writer
  in
  let size_normal, unused_normal = verify_identity ~mode:Ocaml_protoc_plugin.Writer.Balanced v_plugin in
  let size_speed, unused_speed = verify_identity ~mode:Ocaml_protoc_plugin.Writer.Speed v_plugin in
  let size_space, unused_space = verify_identity ~mode:Ocaml_protoc_plugin.Writer.Space v_plugin in
  let data_plugin = Plugin.M.to_proto' (Ocaml_protoc_plugin.Writer.init ()) v_plugin |> Ocaml_protoc_plugin.Writer.contents  in
  let v_plugin' = Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create data_plugin) in
  assert (Poly.equal v_plugin v_plugin');
  let v_protoc = Protoc.decode_pb_m (Pbrt.Decoder.of_string data_plugin) in
  let protoc_encoder = Pbrt.Encoder.create () in
  let () = Protoc.encode_pb_m v_protoc protoc_encoder in
  let data_protoc = Pbrt.Encoder.to_string protoc_encoder in
  let v_plugin'' = Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create data_protoc) in
  let () = match Plugin.M.equal v_plugin v_plugin'' with
    | true -> ()
    | false ->
       eprintf "Orig: %s\n" (Plugin.M.show v_plugin);
       eprintf "New: %s\n" (Plugin.M.show v_plugin');
       failwith "Data not the same"
  in
  printf "%-16s: %5d+%-5d(B) / %5d+%-5d(S) / %5d+%-5d(Sp) - %5d\n%!" (Plugin.M.name' ())
    size_normal unused_normal size_speed unused_speed size_space unused_space (String.length data_protoc);


  let open Bechamel in
    let test_encode =
    Test.make_grouped ~name:"Encode"
      [
        Test.make ~name:"Plugin" (Staged.stage @@ fun () -> Plugin.M.to_proto' Ocaml_protoc_plugin.Writer.(init ()) v_plugin);
        Test.make ~name:"Protoc" (Staged.stage @@ fun () -> let encoder = Pbrt.Encoder.create () in Protoc.encode_pb_m v_protoc encoder; Pbrt.Encoder.to_string encoder)
      ]
  in
  let test_decode =
    Test.make_grouped ~name:"Decode"
      [
        Test.make ~name:"Plugin" (Staged.stage @@ fun () -> Plugin.M.from_proto_exn (Ocaml_protoc_plugin.Reader.create data_plugin));
        Test.make ~name:"Protoc" (Staged.stage @@ fun () -> Protoc.decode_pb_m (Pbrt.Decoder.of_string data_protoc))
      ]
  in
  Test.make_grouped ~name:(Plugin.M.name' ()) [test_encode; test_decode]

let make_int_tests vl =
  let open Ocaml_protoc_plugin in
  let open Bechamel in
  let make_test id group name ?(reset=(fun _ -> ())) f v =
    Test.make ~name:(Printf.sprintf "%s(0x%08Lx)/%s" group id name) (Staged.stage @@ (fun () -> reset v; f v |> Sys.opaque_identity))
  in
  let v = Int64.to_int_exn vl in
  let buffer = Bytes.create 10 in
  let reader =
    let writer = Writer.init () in
    Writer.write_varint_value vl writer;
    Reader.create (Writer.contents writer)
  in
  [
      make_test vl "Read" "boxed" ~reset:(fun r -> Reader.reset r 0) Reader.read_varint reader;
      make_test vl "Read" "unboxed" ~reset:(fun r -> Reader.reset r 0) Reader.read_varint_unboxed reader;
      make_test vl "Write" "boxed" (Writer.write_varint buffer ~offset:0) vl;
      make_test vl "Write" "unboxed" (Writer.write_varint_unboxed buffer ~offset:0) v;
  ]

let _ =
  Random.init 0;
  let module Gc = Stdlib.Gc in
  Gc.full_major ();
  let control = Gc.get () in
  Gc.set { control with minor_heap_size=4000_1000; space_overhead=500 }


let random_list ~len ~f () =
  List.init len ~f:(fun _ -> f ())

let random_string ~len () =
  String.init len ~f:(fun _ -> Random.char ())

let create_test_data ~depth () =
  let module M = Plugin.Bench.M in
  let module Data = Plugin.Bench.Data in
  let module Enum = Plugin.Bench.Enum in
  let optional ~f () =
    match (Random.int 4 = 0) with
    | true -> None
    | false -> Some (f ())
  in
  let create_data () =

    let random_enum () =
      Array.random_element_exn [| Enum.EA; Enum.EB; Enum.EC; Enum.ED; Enum.EE; |]
    in
    let s1 = random_string ~len:20 () in
    let n1 = random_list ~len:100 ~f:(fun () -> Random.int 1_000) () in
    let n2 = random_list ~len:100 ~f:(fun () -> Random.int 1_000) () in
    let d1 = random_list ~len:100 ~f:(fun () -> Random.float 1_000.) () in
    let n3 = Random.int 10 in
    let b1 = Random.bool () in
    let e = random_list ~len:100 ~f:random_enum () in

    Data.make ~s1 ~n1 ~n2 ~d1 ~n3 ~b1 (* ~e *) ()
  in

  let rec create_btree n () =
    match n with
    | 0 -> None
    | n ->
      let data = random_list ~len:2 ~f:create_data () in
      let children =
        random_list ~len:2 ~f:(create_btree (n - 1)) () |> List.filter_opt
      in
      M.make ~children ~data () |> Option.some
  in
  create_btree depth ()

let benchmark tests =
  let open Bechamel in
  let instances = [ meassure ] in
  let cfg = Benchmark.cfg ~compaction:false ~kde:(Some 1) ~quota:(Time.second 5.0) () in
  Benchmark.all cfg instances tests

let analyze results =
  let open Bechamel in
  let ols = Analyze.ols ~bootstrap:5 ~r_square:true
    ~predictors:[| Measure.run |] in
  let results = Analyze.all ols meassure results in
  Analyze.merge ols [ meassure ] [ results ]

let print_bench_results results =
  let open Bechamel in
  let () = Bechamel_notty.Unit.add
             meassure
             (Measure.unit meassure)
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
  let v_plugin = create_test_data ~depth:4 () in
  let v_plugin = Option.value_exn v_plugin in
  [
    make_tests (module Protoc.Bench) (module Plugin.Bench) v_plugin;
    make_tests (module Protoc.Int64) (module Plugin.Int64) 27;
    make_tests (module Protoc.Float) (module Plugin.Float) 27.0001;
    make_tests (module Protoc.String) (module Plugin.String) "Benchmark";
    make_tests (module Protoc.Enum) (module Plugin.Enum) Plugin.Enum.Enum.ED;
    make_tests (module Protoc.Empty) (module Plugin.Empty) ();

    List.init 1000 ~f:(fun i -> i) |> make_tests (module Protoc.Int64_list) (module Plugin.Int64_list);
    List.init 1000 ~f:(fun i -> Float.of_int i) |> make_tests (module Protoc.Float_list) (module Plugin.Float_list);
    List.init 1000 ~f:(fun _ -> random_string ~len:20 ()) |> make_tests (module Protoc.String_list) (module Plugin.String_list);
    (* random_list ~len:100 ~f:(fun () -> Plugin.Enum_list.Enum.ED) () |> make_tests (module Protoc.Enum_list) (module Plugin.Enum_list); *)
       (Bechamel.Test.make_grouped ~name:"Varint" @@ (make_int_tests (0xFFFF_FFFFL)))
  ]
  |> List.iter ~f:(fun test ->
    test
    |> benchmark
    |> analyze
    |> print_bench_results
  )
