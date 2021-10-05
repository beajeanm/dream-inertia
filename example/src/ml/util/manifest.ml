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

let base_path = "/build/"

type entries = {file: string; css: string list}

let decode_entry json =
  let try_decode () =
    let open Yojson.Safe in
    let file = Util.to_string @@ Util.member "file" json in
    let css =
      List.map ~f:Util.to_string @@ Util.to_list @@ Util.member "css" json
    in
    {file; css}
  in
  try Some (try_decode ()) with _ -> None

let load_manifest () =
  let manifest_file =
    Option.value_exn ~message:"Missing manifest file"
      (Assets.Build.read "manifest.json")
  in
  let _manifest_json = Yojson.Safe.from_string manifest_file in
  List.Assoc.map ~f:decode_entry @@ Yojson.Safe.Util.to_assoc _manifest_json

let js_path source =
  let manifest = load_manifest () in
  let entry =
    Option.value_exn ~message:"Missing file"
      (List.Assoc.find_exn ~equal:String.equal manifest source)
  in
  base_path ^ entry.file

let js source =
  let path = js_path source in
  {|<script type="module" src="|} ^ path ^ {|" defer></script>|}

let css_paths source =
  let manifest = load_manifest () in
  let entry =
    Option.value_exn ~message:"Missing file"
      (List.Assoc.find_exn ~equal:String.equal manifest source)
  in
  List.map ~f:(fun str -> base_path ^ str) entry.css

let css source =
  let paths = css_paths source in
  String.concat ~sep:"\n"
  @@ List.map
       ~f:(fun path -> {| <link rel="stylesheet" href="|} ^ path ^ {|"/>|})
       paths
