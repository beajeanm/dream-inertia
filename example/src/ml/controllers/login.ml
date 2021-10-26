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

let get _req =
  Lwt.return @@ Dream_inertia.create_page ~props:(`Assoc []) "Login"

let process_form req =
  let not_empty inputs key =
    match List.Assoc.find ~equal:String.equal inputs key with
    | Some value when not (String.is_empty value) ->
        Ok (key, value)
    | _ ->
        Error (key, key ^ " should not be empty")
  in
  let json_string = function
    | `Null ->
        ""
    | `String x ->
        x
    | _ ->
        failwith "invalid form data"
  in
  let%lwt body = Dream.body req in
  let json = Yojson.Basic.from_string body in
  let json_values =
    List.Assoc.map ~f:json_string @@ Yojson.Basic.Util.to_assoc json
  in
  let handler () =
    let values =
      Result.combine_errors
        [not_empty json_values "username"; not_empty json_values "password"]
    in
    match values with
    | Ok _values ->
        Dream.redirect req "/"
    | Error errors ->
        List.iter
          ~f:(fun (category, value) ->
            Dream.put_flash ("errors." ^ category) value req )
          errors ;
        Dream.redirect req "/login"
  in
  handler ()

let routes inertia =
  [ Dream_inertia.route_to_dream inertia @@ Dream_inertia.get "/login" get
  ; Dream.post "/login" @@ process_form ]
