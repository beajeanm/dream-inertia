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

type page =
  { component: string
  ; props: Yojson.Safe.t
  ; status: Dream.status option
  ; headers: (string * string) list }

type inertia = {template: Yojson.Safe.t -> string; version: string option}

type handler = Dream.request -> page Lwt.t

type route = Dream.method_ * string * handler

let log = Dream.sub_log "inertia"

let create_page ?(props = `Assoc []) ?(status = None) ?(headers = [])
    component_name =
  {component= component_name; props; status; headers}

let add_header data_page name value =
  let headers = List.concat [[(name, value)]; data_page.headers] in
  {data_page with headers}

let init ~version ~template () = {template; version}

let data_page_to_json data_page path version (errors : Yojson.Safe.t option) =
  let errors =
    Option.value ~default:(`Assoc [])
    @@ Option.map ~f:(fun err -> `Assoc [("errors", err)]) errors
  in
  let props = Yojson.Safe.Util.combine data_page.props errors in
  `Assoc
    [ ("component", `String data_page.component)
    ; ("props", props)
    ; ("url", `String path)
    ; ("version", `String version) ]

let get_version inertia = Option.value inertia.version ~default:"1"

let get_errors request =
  let flash_messages = Dream.flash request in
  let errors_flash_messages =
    List.filter
      ~f:(fun (category, _) -> String.is_prefix ~prefix:"errors." category)
      flash_messages
  in
  let error_message_json value =
    try Yojson.Safe.from_string value with _ -> `String value
  in
  let errors_messages =
    List.map
      ~f:(fun (category, value) ->
        ( String.chop_prefix_if_exists ~prefix:"errors." category
        , error_message_json value ) )
      errors_flash_messages
  in
  match errors_messages with [] -> None | _ -> Some (`Assoc errors_messages)

let is_inertia request =
  String.equal "true" @@ String.lowercase @@ Option.value ~default:""
  @@ Dream.header "X-Inertia" request

let render inertia request data_page =
  let path = String.concat ~sep:"/" @@ List.concat [[""]; Dream.path request] in
  let version = get_version inertia in
  let errors = get_errors request in
  let render_html () =
    let data_page = add_header data_page "X-Inertia" "true" in
    let status = Option.value data_page.status ~default:`OK in
    Dream.html ~headers:data_page.headers ~status
    @@ inertia.template
    @@ data_page_to_json data_page path version errors
  in
  let render_json () =
    let data_page = add_header data_page "X-Inertia" "true" in
    let status = Option.value data_page.status ~default:`OK in
    Dream.json ~headers:data_page.headers ~status
    @@ Yojson.Safe.to_string
    @@ data_page_to_json data_page path version errors
  in
  if is_inertia request then render_json () else render_html ()

let make_route method_ path handler = (method_, path, handler)

let get = make_route `GET

let head = make_route `HEAD

let post = make_route `POST

let put = make_route `PUT

let delete = make_route `DELETE

let connect = make_route `CONNECT

let options = make_route `OPTIONS

let trace = make_route `TRACE

let patch = make_route `PATCH

let route_to_dream inertia route =
  let to_dream_handler fn request =
    let open Lwt.Infix in
    fn request >>= fun data -> render inertia request data
  in
  let method_, path, handler = route in
  let dream_handler = to_dream_handler handler in
  match Dream.normalize_method method_ with
  | `GET ->
      Dream.get path dream_handler
  | `HEAD ->
      Dream.head path dream_handler
  | `POST ->
      Dream.post path dream_handler
  | `PUT ->
      Dream.put path dream_handler
  | `DELETE ->
      Dream.delete path dream_handler
  | `CONNECT ->
      Dream.connect path dream_handler
  | `OPTIONS ->
      Dream.options path dream_handler
  | `TRACE ->
      Dream.trace path dream_handler
  | `PATCH ->
      Dream.patch path dream_handler
  | `Method m ->
      failwith ("Unsupported method " ^ m)

let inertia_versionning inertia handler =
  let current_vesion = get_version inertia in
  fun request ->
    let request_version = Dream.header "X-Inertia-Version" request in
    let target = Dream.target request in
    match (Dream.method_ request, request_version) with
    | `GET, Some v when not (String.equal v current_vesion) ->
        log.info (fun log ->
            log "Requested version '%s' does not match current version '%s'" v
              current_vesion ) ;
        Dream.empty ~headers:[("X-Inertia-Location", target)] `Conflict
    | _ ->
        handler request

let error_template inertia component (error : Dream.error) debug_info
    suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status in
  let debug_info = Option.value ~default:"" debug_info in
  let data_page =
    create_page ~status:(Some status)
      ~props:(`Assoc [("status", `Int code); ("debug_info", `String debug_info)])
      component
  in
  let inertia_response request = render inertia request data_page in
  Option.value ~default:(Dream.empty status)
  @@ Option.map ~f:inertia_response error.request
