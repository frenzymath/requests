module

public meta import Init
public meta import Lean.Data.Json

open Lean

public meta section

namespace Requests

inductive RequestError where
  /-- A header name is not a valid token, or its value contains a line break. -/
  | invalidHeader (name : String) (reason : String)
  /-- The backend got no usable response (DNS, TLS, timeout, ...). -/
  | transport (msg : String)
  /-- The server answered with a non-2xx status. Only `Response.ensureOk`
  raises this. -/
  | http (status : Nat) (body : ByteArray)
  /-- The response body is not UTF-8 encoded JSON. -/
  | parse (msg : String) (body : ByteArray)
  /-- The response JSON does not match the expected type. Carries the parsed
  JSON so callers can inspect it or fall back to another type. -/
  | decode (msg : String) (json : Json)

/-- Cut `s` to at most `n` characters so error messages stay readable. -/
def truncate (s : String) (n : Nat := 500) : String :=
  if s.length ≤ n then s else s!"{s.take n}…"

/-- A short, printable excerpt of a response body for error messages. -/
def preview (body : ByteArray) : String :=
  match String.fromUTF8? body with
  | none => s!"<{body.size} bytes of binary data>"
  | some s => truncate s

instance : ToString RequestError where
  toString
    | .invalidHeader name reason => s!"Invalid header {name.quote}: {reason}"
    | .transport msg => s!"Request failed: {msg}"
    | .http status body => s!"HTTP {status}: {preview body}"
    | .parse msg body => s!"Response is not valid JSON: {msg}\n{preview body}"
    | .decode msg json => s!"Unexpected response JSON: {msg}\n{truncate json.compress}"

abbrev RequestIO := ExceptT RequestError IO

/-- Run a request in `IO`, turning a `RequestError` into an `IO.userError`. -/
def RequestIO.toIO {α : Type} (x : RequestIO α) : IO α := do
  IO.ofExcept (← x.run)

/-- Lets `#eval` run `RequestIO` actions directly. -/
instance : MonadEval RequestIO IO := ⟨RequestIO.toIO⟩

end Requests
