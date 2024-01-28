(** Merge a two values. Need to match on the spec to merge messages recursivly *)
let merge: type t. t Spec.Deserialize.compound -> t -> t -> t = fun spec t t' -> match spec with
  | Spec.Deserialize.Basic (_field, Message (_, _), _) -> failwith "Messages with defaults cannot happen"
  | Spec.Deserialize.Basic (_field, _spec, default) when t' = default -> t
  | Spec.Deserialize.Basic (_field, _spec, _) -> t'

    (* The spec states that proto2 required fields must be transmitted exactly once.
       So merging these fields is not possible. The essentially means that you cannot merge
       proto2 messages containing required fields.
       In this implementation, we choose to ignore this, and adopt 'keep last'
    *)
  | Spec.Deserialize.Basic_req (_field, Message (_, merge)) -> merge t t'
  | Spec.Deserialize.Basic_req (_field, _spec) -> t'
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
