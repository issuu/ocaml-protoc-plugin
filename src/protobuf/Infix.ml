module Int64 = struct
  open Int64
  let (land) = logand
  let (lsl) = shift_left
  let (lsr) = shift_right_logical
  let (lor) = logor
  let (lxor) = logxor
  let (+) = add
  let (/) = div
  let ( * ) = mul
  let (-) = sub
end
