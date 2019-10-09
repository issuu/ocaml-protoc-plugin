module Int64 = struct
  open Int64
  let (land) = logand
  let (lsl) = shift_left
  let (lsr) = shift_right
  let (lor) = logor
  let (lxor) = logxor
  let (+) = add
  let (/) = div
  let ( * ) = mul
  let (-) = sub
  let (%) x y =
    let rval = rem x y in
    if rval < zero then rval + y else rval
end
module Int = struct
  let (+) = (+)
end
module Result = struct
  let ( >>| ) res f = Result.map f res
  let ( >>= ) res f = Result.bind f res
end
