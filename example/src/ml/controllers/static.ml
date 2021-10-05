(******************************************************************************)
(* MIT License                                                                *)
(*                                                                            *)
(* Copyright (c) 2021 Jean-Michel Bea                                         *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining a    *)
(* copy of this software and associated documentation files (the "Software"), *)
(* to deal in the Software without restriction, including without limitation  *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *)
(* and/or sell copies of the Software, and to permit persons to whom the      *)
(* Software is furnished to do so, subject to the following conditions:       *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software.                        *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *)
(* DEALINGS IN THE SOFTWARE.                                                  *)
(******************************************************************************)

open Base

let routes =
  let crunch_build_loader _directory path _request =
    match Assets.Build.read path with
    | Some data ->
        Dream.respond data
    | None ->
        Dream.empty `Not_Found
  in
  let crunch_static_loader _directory path _request =
    match Assets.Static.read path with
    | Some data ->
        Dream.respond data
    | None ->
        Dream.empty `Not_Found
  in
  [ Dream.get "/favicon.png" @@ Dream.from_filesystem "public" "favicon.png"
  ; Dream.get "/build/**" @@ Dream.static ~loader:crunch_build_loader ""
  ; Dream.get "/static/**" @@ Dream.static ~loader:crunch_static_loader "" ]
