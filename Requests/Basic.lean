module

public meta import Requests.Url
public meta import Requests.Backend.Curl
public meta import Lean.Data.Json

/-! # Frontend

Turns a call like `get url params` into a validated `Request`, hands it to the
backend (`Curl.send`), then checks the status and decodes the JSON response.
Nothing here depends on how the backend performs the request. -/

open Lean

public meta section

namespace Requests

def acceptJson : Headers := #[("Accept", "application/json")]

def sendJson : Headers := acceptJson.push ("Content-Type", "application/json")

variable {α β : Type} [ToJson α] [FromJson β]

/-- Main entry point: send a request with an optional JSON `body` and decode
the JSON response. `params` become the query string. User `headers` replace
defaults of the same name. Non-2xx responses fail with `.http`, raised by
`Response.ensureOk`. -/
def request (method : Method) (url : String) (params : Params := #[])
    (headers : Headers := #[]) (body : Option Json := none)
    (timeout : Nat := 30) : RequestIO β := do
  let headers := (if body.isSome then sendJson else acceptJson).merge headers
  validateHeaders headers
  let res ← Curl.send {
    method, headers, timeout
    url := withQuery url (encodeQuery params)
    body := body.map (·.compress.toUTF8) }
  res.ensureOk >>= Response.json

/-- GET `url` with `params` as the query string. -/
def get (url : String) (params : Params := #[]) (headers : Headers := #[])
    (timeout : Nat := 30) : RequestIO β :=
  request .get url params headers (timeout := timeout)

/-- POST `data` as a JSON body to `url`. -/
def post (url : String) (data : α) (headers : Headers := #[])
    (timeout : Nat := 30) : RequestIO β :=
  request .post url (headers := headers) (body := toJson data) (timeout := timeout)

end Requests
