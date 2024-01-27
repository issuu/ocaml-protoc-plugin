(** Merge a two values. Need to match on the spec to merge messages recursivly *)
let merge: type t. t Spec.Deserialize.compound -> t -> t -> t = fun spec t t' -> match spec with
  | Spec.Deserialize.Basic (_field, Message (_, merge), _) -> merge t t'
  | Spec.Deserialize.Basic (_field, _spec, Some default) when t' = default -> t
  | Spec.Deserialize.Basic (_field, _spec, _) -> t'
  | Spec.Deserialize.Basic_opt (_field, Message (_, merge)) ->
    begin
      match t, t' with
      | None, None -> None
      | Some t, None -> Some t
      | None, Some t -> Some t
      | Some t, Some t' -> Some (merge t t')
    end
  | Spec.Deserialize.Basic_opt (_field, _spec) -> begin
      match t' with
      | Some _ -> t'
      | None -> t
    end
  | Spec.Deserialize.Repeated (_field, _, _) -> t @ t'
  (* | Spec.Deserialize.Oneof _ when t' = `not_set -> t *)
  | Spec.Deserialize.Oneof _ -> failwith "Implementation is part of generated code"
