module Plugin = struct
  include Plugin_types
  module Pb = Plugin_pb
  module Pp = Plugin_pp
end

module Descriptor = struct
  include Descriptor_types
  module Pb = Descriptor_pb
  module Pp = Descriptor_pp
end
