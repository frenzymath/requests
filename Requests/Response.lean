module

public meta import Requests.Error
public meta import Lean.Data.Json

open Lean

public meta section

namespace Requests

/-- A response as returned by the backend, whatever its status. -/
structure Response where
  status : Nat
  body   : ByteArray

namespace Response

def ok (r : Response) : Bool :=
  200 ≤ r.status && r.status < 300

variable {m : Type → Type} [Monad m] [MonadExceptOf RequestError m]

/-- Fail with `.http` unless the status is 2xx. This is the only place that
raises `.http`. -/
def ensureOk (r : Response) : m Response :=
  if r.ok then pure r else throw (.http r.status r.body)

/-- Decode the body as JSON, then as `β`. -/
def json {β : Type} [FromJson β] (r : Response) : m β :=
  MonadExcept.ofExcept do
    let some text := String.fromUTF8? r.body
      | throw (.parse "body is not valid UTF-8" r.body)
    let json ← Json.parse text |>.mapError (.parse · r.body)
    fromJson? json |>.mapError (.decode · json)

end Response

end Requests
