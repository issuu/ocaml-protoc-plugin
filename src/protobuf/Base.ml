module List = ListLabels
module String = StringLabels
module Bytes = BytesLabels
module Option = struct
  let value ~default = function
    | None -> default
    | Some x -> x
  let some x = Some x
end
module Result = struct
  type ('a, 'b) t = ('a, 'b) result
    let map ~f = function
      | Ok x -> Ok (f x)
      | Error err -> Error err
    let bind ~f = function
      | Ok x -> f x
      | Error err -> Error err
    let return x = Ok x
    let (>>|) res f = map ~f res
    let (>>=) res f = bind ~f res
    let ok_unit = Ok ()
    let fail x = Error x
end
module Int64 = struct
  include Int64
  let (land) = logand
  let (lsl) = shift_left
  let (lsr) = shift_right
  let (lor) = logor
  let (lxor) = logxor
  let (+) = add
  let (/) = div
  let (%) = rem
  let ( * ) = mul
  let ( - ) = sub
  let is_positive t = compare t zero > 0
  let is_negative t = compare t zero < 0
end
module Int = struct
  let (+) = (+)
end
