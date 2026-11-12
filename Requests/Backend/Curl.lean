module

public meta import Requests.Request
public meta import Requests.Response

/-! # curl backend

Performs a `Request` by running the `curl` executable. A backend only moves
bytes: it sends the request as given and returns the status and body it got.
URL building, header defaults and validation, and interpreting the response all
belong to the frontend (`Requests.Basic`). Any other backend has to provide the
same `send : Request → RequestIO Response`. -/

public meta section

namespace Requests.Curl

/-- Quote a value for a curl config file. -/
def curlQuote (s : String) : String :=
  "\"" ++ s.foldl (· ++ escape ·) "" ++ "\""
where
  escape : Char → String
    | '\\' => "\\\\"
    | '"' => "\\\""
    | '\n' => "\\n"
    | '\r' => "\\r"
    | '\t' => "\\t"
    | c => c.toString

/-- curl config for `r`. The body itself is not included; it is read from stdin. -/
def config (r : Request) : String :=
  let opt (k v : String) := s!"{k} = {curlQuote v}\n"
  -- `Name;` sends an empty header; `Name:` would make curl drop it.
  let header : String × String → String
    | (k, "") => opt "header" s!"{k};"
    | (k, v) => opt "header" s!"{k}: {v}"
  String.join <|
    ["silent\n", "show-error\n", "globoff\n",
     opt "request" (toString r.method),
     opt "url" r.url,
     opt "max-time" (toString r.timeout),
     -- Appended to stdout after the body, always exactly three digits.
     opt "write-out" "%{http_code}"]
    ++ r.headers.toList.map header
    ++ (r.body.map fun _ => opt "data-binary" "@-").toList

structure CurlOutput where
  exitCode : UInt32
  stdout   : ByteArray
  stderr   : String

/-- Run curl with `config` and feed `body` to its stdin.

The config goes through a temporary file (created with mode 0600) and the body
through stdin, so neither the URL, the headers nor the body appear in the
process list, and bodies are not bound by argument or config line limits. -/
def run (config : String) (body : Option ByteArray) : IO CurlOutput :=
  IO.FS.withTempFile fun h path => do
    h.putStr config
    h.flush
    -- `-q` must come first: it stops curl from reading the user's ~/.curlrc.
    let child ← IO.Process.spawn {
      cmd := "curl", args := #["-q", "--config", path.toString]
      stdin := if body.isSome then .piped else .null
      stdout := .piped, stderr := .piped
    }
    let (stdin, child) ← child.takeStdin
    -- curl reads `@-` completely before sending anything, so writing it all
    -- up front cannot deadlock. Dropping `stdin` closes the pipe.
    if let some b := body then
      stdin.write b
      stdin.flush
    let stdout ← IO.asTask child.stdout.readBinToEnd .dedicated
    let stderr ← child.stderr.readToEnd
    let exitCode ← child.wait
    let stdout ← IO.ofExcept stdout.get
    return { exitCode, stdout, stderr }

/-- Perform `r` and return the status and body.

`r.headers` must already have passed `validateHeaders`: curl turns an escaped
line break in a header value back into a real one. -/
def send (r : Request) : RequestIO Response := do
  let out ← run (config r) r.body
  if out.exitCode != 0 then
    throw (.transport s!"curl exit {out.exitCode}: {out.stderr.trimRight}")
  let n := out.stdout.size - 3
  let some status := String.fromUTF8? (out.stdout.extract n out.stdout.size) >>= String.toNat?
    | throw (.transport "curl did not report an HTTP status")
  return { status, body := out.stdout.extract 0 n }

end Requests.Curl
