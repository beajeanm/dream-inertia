type t = { version : string; js_path : string; css_path : string }

let make ~version ~js_path ~css_path () = { version; js_path; css_path }

type page = {
  (* The name of view template in view/src/Pages *)
  component : string;
  (* The data to create the vue component *)
  props : Yojson.Safe.t;
  url : string;
  headers : (string * string) list;
  status : Dream.status;
}

type handler = Dream.request -> page Lwt.t

let page ~component ~props ~url ?(headers = []) ?(status = `OK) () =
  { component; props; url; headers; status }

let page_to_string version page =
  let json =
    `Assoc
      [
        ("component", `String page.component);
        ("props", page.props);
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
    (Dream.html_escape @@ page_to_string version page)
    js

let update_status = function `Found -> `See_Other | status -> status

let add_location headers status url =
  match status with
  | `Conflict -> ("X-Inertia-Location", url) :: headers
  | _ -> headers

let to_dream_handler inertia handler =
  let open Lwt.Syntax in
  fun req ->
    let version =
      Dream.header req "X-Inertia-Version"
      |> Option.value ~default:inertia.version
    in
    let meth = Dream.method_ req in
    if meth = `GET && not (String.equal version inertia.version) then
      Dream.empty
        (* TODO: How to handle given back a URL since Dream.path is deprecated... *)
        ~headers:[ ("X-Inertia", "true"); ("X-Inertia-Location", "/") ]
        `Conflict
    else
      let* page = handler req in
      let status = update_status page.status in
      let headers = ("X-Inertia", "true") :: page.headers in
      let headers = add_location headers status page.url in
      match Dream.header req "X-Inertia" with
      | Some "true" ->
          Dream.json ~status ~headers @@ page_to_string inertia.version page
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
