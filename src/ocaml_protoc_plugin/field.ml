type t =
  | Varint of Int64.t (* int32, int64, uint32, uint64, sint32, sint64, bool, enum *)
  | Fixed_64_bit of Int64.t (* fixed64, sfixed64, double *)
  | Length_delimited of {
      offset : int;
      length : int;
      data : string;
    } (* string, bytes, embedded messages, packed repeated fields *)
  | Fixed_32_bit of Int32.t (* fixed32, sfixed32, float *)

let to_yojson = function
  | Varint a0 -> `List [ `String "Field.Varint"; `Intlit (Int64.to_string a0) ]
  | Fixed_64_bit a0 ->
      `List [ `String "Field.Fixed_64_bit"; `Intlit (Int64.to_string a0) ]
  | Length_delimited { offset; length; data } ->
      `List
        [
          `String "Field.Length_delimited";
          `Assoc
            [
              ("offset", `Int offset);
              ("length", `Int length);
              ("data", `String data);
            ];
        ]
  | Fixed_32_bit a0 ->
      `List [ `String "Field.Fixed_32_bit"; `Intlit (Int32.to_string a0) ]

let yojson_of_t = to_yojson

let of_yojson = function
  | `List [ `String "Field.Varint"; `Intlit a0 ] -> Varint (Int64.of_string a0)
  | `List [ `String "Field.Fixed_64_bit"; `Intlit a0 ] ->
      Fixed_64_bit (Int64.of_string a0)
  | `List
      [
        `String "Field.Length_delimited";
        `Assoc
          [
            ("offset", `Int offset);
            ("length", `Int length);
            ("data", `String data);
          ];
      ] ->
      Length_delimited { offset; length; data }
  | `List [ `String "Field.Fixed_32_bit"; `Intlit a0 ] ->
      Fixed_32_bit (Int32.of_string a0)
  | _ -> failwith "field is malformed"

let t_of_yojson = of_yojson

let varint v = Varint v
let fixed_32_bit v = Fixed_32_bit v
let fixed_64_bit v = Fixed_64_bit v
let length_delimited ?(offset=0) ?length data =
  let length = Option.value ~default:(String.length data - offset) length in
  Length_delimited {offset; length; data}


let pp: Format.formatter -> t -> unit = fun fmt ->
  function
  | Varint a0 ->
    (Format.fprintf fmt "(@[<2>Field.Varint@ ";
     (Format.fprintf fmt "%LdL") a0;
     Format.fprintf fmt "@])")
  | Fixed_64_bit a0 ->
    (Format.fprintf fmt
       "(@[<2>Field.Fixed_64_bit@ ";
     (Format.fprintf fmt "%LdL") a0;
     Format.fprintf fmt "@])")
  | Length_delimited
      { offset = aoffset; length = alength; data = adata } ->
    (Format.fprintf fmt
       "@[<2>Field.Length_delimited {@,";
     (((Format.fprintf fmt "@[%s =@ " "offset";
        (Format.fprintf fmt "%d") aoffset;
        Format.fprintf fmt "@]");
       Format.fprintf fmt ";@ ";
       Format.fprintf fmt "@[%s =@ " "length";
       (Format.fprintf fmt "%d") alength;
       Format.fprintf fmt "@]");
      Format.fprintf fmt ";@ ";
      Format.fprintf fmt "@[%s =@ " "data";
      (Format.fprintf fmt "%S") adata;
      Format.fprintf fmt "@]");
     Format.fprintf fmt "@]}")
  | Fixed_32_bit a0 ->
    (Format.fprintf fmt
       "(@[<2>Field.Fixed_32_bit@ ";
     (Format.fprintf fmt "%ldl") a0;
     Format.fprintf fmt "@])")

let show : t -> string = Format.asprintf "%a" pp
