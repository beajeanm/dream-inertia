type t = { version : string; root_view : string -> string }
type json = Yojson.Safe.t

type prop =
  | Regular of json
  | Delay of (unit -> json)
  | Lazy of (unit -> json)
  | Always of json

type page = {
  (* The name of view template in view/src/Pages *)
  component : string;
  (* The data to create the vue component *)
  props : (string * prop) list;
  url : string;
}

let prop json = Regular json
let always_prop json = Always json
let lazy_prop json = Lazy json
let delayed_prop json = Delay json
let init ~version ~root_view () = { version; root_view }
let page ~component ?(props = []) ~url () = { component; props; url }
let with_prop page key prop = { page with props = (key, prop) :: page.props }
let with_url page url = { page with url }

let is_inertia_request request =
  Dream.header request "X-Inertia" |> Option.is_some

let is_redirect response =
  let status = Dream.status response in
  status = `Moved_Permanently || status = `Found

let request_path request =
  begin
    Dream.to_path (Dream.path request)
  end
  [@alert "-deprecated"]

let set_location request response =
  if Dream.is_redirection (Dream.status response) then
    Dream.set_header response "Location" (request_path request)
  else ()

let set_csrf_cookie request response =
  let valid_for = Ptime.Span.of_float_s 3600. |> Option.get in
  let csrf = Dream.csrf_token request in
  Dream.set_cookie ~encrypt:false ~http_only:false
    ~expires:
      Ptime.(
        add_span (Ptime_clock.now ()) valid_for
        |> Option.get |> Ptime.to_float_s)
    response request "XSRF-TOKEN" csrf

let is_non_post_redirect request response =
  let meth = Dream.method_ request in
  is_redirect response && (meth = `PUT || meth = `PATCH || meth = `DELETE)

let update_status (response : Dream.response) =
  let open Lwt.Syntax in
  let* body = Dream.body response in
  Dream.respond ~status:`See_Other ~headers:(Dream.all_headers response) body

let process_response request inner_handler =
  let open Lwt.Syntax in
  let* response = inner_handler request in
  set_location request response;
  set_csrf_cookie request response;
  if is_non_post_redirect request response then update_status response
  else Lwt.return response

let stale_response request =
  Dream.empty
    ~headers:[ ("X-Inertia-Location", request_path request) ]
    `Conflict

let middleware inertia inner_handler request =
  let version =
    Dream.header request "X-Inertia-Version" |> Option.value ~default:""
  in
  match
    ( Dream.method_ request,
      is_inertia_request request,
      String.equal version inertia.version )
  with
  | `GET, true, false -> stale_response request
  | _, _, _ -> process_response request inner_handler

let full_page_filter named_props =
  let not_lazy = function Lazy _ -> false | _ -> true in
  List.filter (fun (_, prop) -> not_lazy prop) named_props

let inertia_page_filter predicate named_props =
  let filter_prop predicate = function
    | _, Always _ -> true
    | name, _ -> predicate name
  in
  List.filter (filter_prop predicate) named_props

let extract_prop = function
  | Always j -> j
  | Regular j -> j
  | Delay lj -> lj ()
  | Lazy lj -> lj ()

let page_to_string version page filter =
  let filtered_props = filter page.props in
  let merged_props =
    `Assoc
      (List.map (fun (name, prop) -> (name, extract_prop prop)) filtered_props)
  in
  let json =
    `Assoc
      [
        ("component", `String page.component);
        ("props", merged_props);
        ("url", `String page.url);
        ("version", `String version);
      ]
  in
  Yojson.Safe.to_string json

let include_filter req =
  match Dream.header req "X-Inertia-Partial-Data" with
  | Some fields ->
      let fields = String.split_on_char ',' fields in
      Some (fun prop -> List.mem prop fields)
  | _ -> None

let exclude_filter req =
  match Dream.header req "X-Inertia-Partial-Except" with
  | Some fields ->
      let fields = String.split_on_char ',' fields in
      Some (fun prop -> not (List.mem prop fields))
  | _ -> None

let props_filter request =
  match include_filter request with
  | Some filter -> filter
  | None -> exclude_filter request |> Option.value ~default:(fun _ -> true)

let inertia_response inertia ?(headers = []) ?(status = `OK) request page =
  let headers = ("X-Inertia", "true") :: headers in
  if is_inertia_request request then
    let prop_name_filter = props_filter request in
    let page_data =
      page_to_string inertia.version page (inertia_page_filter prop_name_filter)
    in
    Dream.json ~headers ~status page_data
  else
    let page_data = page_to_string inertia.version page full_page_filter in
    Dream.html ~status ~headers @@ inertia.root_view page_data

module Root_View_Helper = struct
  let create ~js ~css page_data =
    Format.asprintf
      {html|
<!doctype html>
<html lang="en">

<head>
  <meta charset="UTF-8" />
  <link rel="icon" href="/favicon.ico" />
  <link rel="stylesheet" href="%s">
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
</head>

<body>
  <div id="app" data-page='%s'></div>
  <script type="module" src="%s"></script>
</body>

</html>
|html}
      css
      (Dream.html_escape @@ page_data)
      js
end
