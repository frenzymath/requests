module

public meta import Requests.Headers

public meta section

namespace Requests

inductive Method where
  | get
  | post
  deriving Repr, BEq

instance : ToString Method where
  toString
    | .get => "GET"
    | .post => "POST"

/-- A request as handed to the backend: URL fully built, headers validated,
body already encoded. -/
structure Request where
  method  : Method
  url     : String
  headers : Headers := #[]
  body    : Option ByteArray := none
  /-- Maximum time in seconds for the whole transfer. `0` disables the limit. -/
  timeout : Nat := 30

end Requests
