import Requests.Future
import Lean

open Lean

namespace Requests

def default_headers : Json :=
  json% {
    "accept" : "application/json",
    "content-type" : "application/json"
  }

def renderParams (params: Json) : IO String :=
  match params with
  | Json.obj kvs =>
    return kvs.fold (fun acc k v => acc ++ #[s!"{k}={
      match Json.getStr? v with
      | Except.error _ => ""
      | Except.ok s => s
      }"]) #[] |>.joinSep "&"
  | _ => throw $ IO.userError "Params must be an object"

def renderHeaders (headers: Json) : IO (Array String) :=
  match headers with
  | Json.obj kvs =>
    return kvs.fold (fun acc k v => acc ++ #["-H", s!"{k}: {
      match Json.getStr? v with
      | Except.error _ => ""
      | Except.ok s => s
      }"]) #[]
  | _ => throw $ IO.userError "Headers must be an object"

def get {α β γ : Type} [ToJson α] [FromJson β] [ToJson γ] (url : String) (params: α) (headers: Json := default_headers) : IO β := do
  let headers ← renderHeaders headers
  let params ← renderParams (toJson params)
  let url := url ++ "?" ++ params
  let out ← IO.Process.output {
    cmd := "curl"
    args := #["-X", "GET", url] ++ headers
  }
  if out.exitCode != 0 then
     throw $ IO.userError s!"Request failed. {out.stderr} {out.stdout}"
  let some json := Json.parse out.stdout |>.toOption
    | throw $ IO.userError s!"Json Parse failed. {out.stderr} {out.stdout}"
  let some res := (fromJson? json : Except String β) |>.toOption
    | throw $ IO.userError s!"From Json failed. {out.stderr} {out.stdout}"
  return res

def post {α β γ : Type} [ToJson α] [FromJson β] [ToJson γ] (url : String) (data : α) (headers: Json := default_headers): IO β := do
  let headers ← renderHeaders headers
  let data := (toJson data).pretty UInt64.size
  let out ← IO.Process.output {
    cmd := "curl"
    args := #["-X", "POST", url] ++ headers ++ #["-d", data]
  }
  if out.exitCode != 0 then
     throw $ IO.userError s!"Request failed. {out.stderr} {out.stdout} {toJson data}"
  let some json := Json.parse out.stdout |>.toOption
    | throw $ IO.userError s!"Json Parse failed. {out.stderr} {out.stdout} {toJson data}"
  let some res := (fromJson? json : Except String β) |>.toOption
    | throw $ IO.userError s!"From Json failed. {out.stderr} {out.stdout} {toJson data}"
  return res

end Requests
