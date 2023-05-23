module type Message = sig
  type t
  val from_proto: Reader.t -> t Result.t
  val to_proto: t -> Writer.t
end

module type Rpc = sig
  module Request : Message
  module Response : Message

  (** gRPC service name as defined by the gRPC http2 spec.
      see https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md#appendix-a---grpc-for-protobuf
  *)
  val name : string

  (** Name of the enclosed package name if any *)
  val package_name : string option

  (** Name of the service in which this method is defined *)
  val service_name : string

  (** Name of the method *)
  val method_name : string
end

let make_client_functions (type req) (type rep)
    ((module Request : Message with type t = req),
     (module Response : Message with type t = rep)) =
  Request.to_proto, Response.from_proto

let make_service_functions (type req) (type rep)
    ((module Request : Message with type t = req),
    (module Response : Message with type t = rep)) =
  Request.from_proto, Response.to_proto
