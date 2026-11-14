module

public meta import Requests.Error

public meta section

namespace Requests

/-- Name/value pairs, kept in order. Repeated names are allowed. -/
abbrev Headers := Array (String × String)

/-- `base` with every header that `override` also names (case-insensitively) replaced. -/
def Headers.merge (base override : Headers) : Headers :=
  let names := override.map (·.1.toLower)
  base.filter (fun (k, _) => !names.contains k.toLower) ++ override

/-- Reject header names that are not valid HTTP tokens and values that would
inject extra header lines or contain other forbidden control characters. -/
def validateHeaders {m : Type → Type} [Monad m] [MonadExceptOf RequestError m]
    (hs : Headers) : m Unit :=
  hs.forM fun (k, v) => do
    unless !k.isEmpty && k.all fun c =>
      (c.val ≥ 48 && c.val ≤ 57) ||
      (c.val ≥ 65 && c.val ≤ 90) ||
      (c.val ≥ 97 && c.val ≤ 122) ||
      "!#$%&'*+-.^_`|~".contains c do
      throw (.invalidHeader k "names must contain only HTTP token characters")
    unless v.all fun c =>
      c.val == 9 || (c.val ≥ 32 && c.val ≤ 126) || c.val ≥ 128 do
      throw (.invalidHeader k "value contains a forbidden control character")

end Requests
