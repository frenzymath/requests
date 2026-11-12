module

public meta import Init

public meta section

namespace Requests

/-- Query parameters, kept in order. Repeated keys are allowed (`a=1&a=2`). -/
abbrev Params := Array (String × String)

/-- RFC 3986 percent-encoding of the UTF-8 bytes of `s`. -/
def percentEncode (s : String) : String :=
  s.toUTF8.foldl (init := "") fun acc b =>
    let c := Char.ofNat b.toNat
    if c.isAlphanum || c ∈ ['-', '.', '_', '~'] then acc.push c
    else acc.push '%' |>.push (hex (b.toNat / 16)) |>.push (hex (b.toNat % 16))
where
  hex (n : Nat) : Char := Char.ofNat (if n < 10 then 48 + n else 55 + n)

def encodeQuery (params : Params) : String :=
  "&".intercalate (params.toList.map fun (k, v) => s!"{percentEncode k}={percentEncode v}")

/-- Append a query string to `url`, respecting an existing query and fragment. -/
def withQuery (url query : String) : String :=
  if query.isEmpty then url else
  match url.splitOn "#" with
  | [] => url
  | base :: fragment =>
    let sep :=
      if !base.contains '?' then "?"
      else if base.endsWith "?" || base.endsWith "&" then ""
      else "&"
    "#".intercalate ((base ++ sep ++ query) :: fragment)

end Requests
