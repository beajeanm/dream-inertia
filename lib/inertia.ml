type t = { version : string; js_path : string; css_path : string }

type page = {
  (* The name of view template in view/src/Pages *)
  component : string;
  (* The data to create the vue component *)
  props : (string * Yojson.Safe.t) list;
  lazy_props : (string * Yojson.Safe.t Lazy.t) list;
  url : string;
  headers : (string * string) list;
  status : Dream.status;
}

type handler = Dream.request -> page Lwt.t

let make ~version ~js_path ~css_path () = { version; js_path; css_path }

let is_redirect response =
  let status = Dream.status response in
  status = `Moved_Permanently || status = `Found

let is_non_post_redirect request response =
  let meth = Dream.method_ request in
  is_redirect response && (meth = `PUT || meth = `PATCH || meth = `DELETE)

let middleware inertia handler request =
  let meth = Dream.method_ request in
  let version =
    Dream.header request "X-Inertia-Version"
    |> Option.value ~default:inertia.version
  in
  let is_stale = meth = `GET && not (String.equal version inertia.version) in
  if is_stale then
    begin
      let path = Dream.to_path (Dream.path request) in
      Dream.empty ~headers:[ ("X-Inertia-Location", path) ] `Conflict
    end
    [@alert "-deprecated"]
  else
    let open Lwt.Syntax in
    let* response = handler request in
    if is_non_post_redirect request response then
      let+ body = Dream.body response in
      Dream.response ~status:`See_Other
        ~headers:(Dream.all_headers response)
        body
    else Lwt.return response

let page ~component ?(props = []) ?(lazy_props = []) ~url ?(headers = [])
    ?(status = `OK) () =
  { component; props; lazy_props; url; headers; status }

let filter_keys assoc predicate =
  List.filter (fun (key, _) -> predicate key) assoc

let page_to_string version page props_filter =
  let filtered_lazy_props =
    Option.map (filter_keys page.lazy_props) props_filter
    |> Option.value ~default:page.lazy_props
  in
  let forced_lazy_props =
    List.map
      (fun (key, lazy_json) -> (key, Lazy.force lazy_json))
      filtered_lazy_props
  in
  let filtered_props =
    Option.map (filter_keys page.props) props_filter
    |> Option.value ~default:page.props
  in
  let merged_props =
    Yojson.Safe.Util.combine (`Assoc filtered_props) (`Assoc forced_lazy_props)
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

let full_page page ~version ~js ~css =
  Format.asprintf
    {|
<!doctype html>
<html lang="en">

<head>
  <meta charset="UTF-8" />
  <link rel="icon" href="/favicon.ico" />
  <link rel="stylesheet" href="%s">
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>Vite App</title>
</head>

<body>
  <div id="app" data-page='%s'></div>
  <script type="module" src="%s"></script>
</body>

</html>
|}
    css
    (Dream.html_escape @@ page_to_string version page None)
    js

let update_status = function `Found -> `See_Other | status -> status

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

let props_filter req =
  match include_filter req with Some _ as f -> f | None -> exclude_filter req

let to_dream_handler inertia handler =
  let open Lwt.Syntax in
  fun req ->
    let* page = handler req in
    let status = update_status page.status in
    let headers = ("X-Inertia", "true") :: page.headers in
    match Dream.header req "X-Inertia" with
    | Some "true" ->
        Dream.json ~status ~headers
        @@ page_to_string inertia.version page (props_filter req)
    | _ ->
        Dream.html ~status ~headers
        @@ full_page page ~version:inertia.version ~css:inertia.css_path
             ~js:inertia.js_path

let to_dream_method delegate inertia path handler =
  let handler = to_dream_handler inertia handler in
  delegate path handler

let get = to_dream_method Dream.get
let post = to_dream_method Dream.post
let put = to_dream_method Dream.put
let delete = to_dream_method Dream.delete
