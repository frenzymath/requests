import Requests

open Lean Requests

#eval (get "https://httpbin.org/get" #[("haha", "haha")] : RequestIO Json)

#eval (post "https://httpbin.org/post" (json% {"haha": "haha"}) : RequestIO Json)
