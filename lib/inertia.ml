open Containers

type t = { mutable version : string; mutable root_view : string -> string }

module Page = struct
  type deferred_prop = {
    group : string;
    key : string;
    callback : unit -> Yojson.Safe.t;
  }

  type t = {
    component : string;
    props : (string * Yojson.Safe.t) list;
    deferred_props : deferred_prop list;
    url : string;
  }
end

let config : t =
  {
    version = "";
    root_view =
      (fun _ ->
        {html|
<html>
  <head>
    <title>No root template set</title>
  </head>
  <body>
    <h1>No root template set</h1>
    <p>You need to set a root template using <code>Inertia.middleware</code> before using Inertia.</p>
    <pre>
      %s
    </pre>
  </body>
  |html});
  }

let defer ?(group = "default") ~key callback = Page.{ group; key; callback }

let page ~component ?(props = []) ?(deferred_props = []) ~url () =
  Page.{ component; props; deferred_props; url }

let middleware ~version ~root_view () =
  config.version <- version;
  config.root_view <- root_view;
  fun inner_handler request ->
    Middleware.create config.version inner_handler request

let classify_request request =
  match Dream.header request "X-Inertia" with
  | None -> `Full_page
  | Some _ -> (
      match
        ( Dream.header request "X-Inertia-Partial-Component",
          Dream.header request "X-Inertia-Partial-Data",
          Dream.header request "X-Inertia-Partial-Except" )
      with
      | Some component, Some fields, _ ->
          `Partial_include (component, String.split_on_char ',' fields)
      | Some component, None, Some fields ->
          `Partial_exclude (component, String.split_on_char ',' fields)
      | _ -> `Inertia)

let flash_key = "inertia-errors"

let get_flash_messages request =
  List.assoc_opt ~eq:String.equal flash_key (Dream.flash_messages request)
  |> Option.map Yojson.Safe.from_string
  |> Option.value ~default:(`Assoc [])

let add_error request data =
  let previous = get_flash_messages request in
  let data = Yojson.Safe.to_string (Yojson.Safe.Util.combine data previous) in
  Dream.add_flash_message request flash_key data

let shared_data_key = "Inertia-shared-data"

let get_shared_data request =
  Dream.session_field request shared_data_key
  |> Option.map Yojson.Safe.from_string
  |> Option.value ~default:(`Assoc [])

let add_shared_data request key prop =
  let current_data = get_shared_data request |> Yojson.Safe.Util.to_assoc in
  let filtered_data =
    List.filter (fun (k, _) -> not (String.equal k key)) current_data
  in
  let updated_data = `Assoc ((key, prop) :: filtered_data) in
  Dream.set_session_field request shared_data_key
    (Yojson.Safe.to_string updated_data)

let error_props_key = "errors"

let merge_data request page =
  let flash_messages = [ (error_props_key, get_flash_messages request) ] in
  let shared_data = get_shared_data request |> Yojson.Safe.Util.to_assoc in
  List.concat [ flash_messages; shared_data; page.Page.props ]

let page_data version component data url deferred =
  let json_data =
    [
      ("component", `String component);
      ("props", data);
      ("url", `String url);
      ("version", `String version);
    ]
  in
  let json_data =
    if List.is_empty deferred then json_data
    else
      let groupped_props =
        List.group_by
          ~eq:(fun dp1 dp2 -> String.equal dp1.Page.group dp2.Page.group)
          deferred
      in
      let props_keys =
        List.map
          (fun props ->
            let group_name = (List.hd props).Page.group in
            let keys = List.map Page.(fun dp -> `String dp.key) props in
            (group_name, `List keys))
          groupped_props
      in
      ("deferredProps", `Assoc props_keys) :: json_data
  in
  Yojson.Safe.to_string (`Assoc json_data)

let extract_deferred_props page fields =
  List.filter
    (fun dprop -> List.mem dprop.Page.key fields)
    page.Page.deferred_props
  |> List.map (fun dprop -> Page.(dprop.key, dprop.callback ()))

let render ?(headers = []) ?(status = `OK) request page =
  let headers = ("Vary", "X-Inertia") :: ("X-Inertia", "true") :: headers in
  let props = merge_data request page in
  match classify_request request with
  | `Full_page ->
      Dream.html ~status ~headers
      @@ config.root_view
           (page_data config.version page.component (`Assoc props) page.url
              page.deferred_props)
  | `Inertia ->
      Dream.json ~status ~headers
      @@ page_data config.version page.component (`Assoc props) page.url
           page.deferred_props
  | `Partial_include (component, fields) ->
      let filtered_props =
        List.filter
          (fun (k, _) -> String.equal k error_props_key || List.mem k fields)
          props
      in
      let all_props =
        List.concat [ extract_deferred_props page fields; filtered_props ]
      in
      Dream.json ~status ~headers
      @@ page_data config.version component (`Assoc all_props) page.url []
  | `Partial_exclude (component, fields) ->
      let filtered_props =
        List.filter (fun (k, _) -> not (List.mem k fields)) props
      in
      Dream.json ~status ~headers
      @@ page_data config.version component (`Assoc filtered_props) page.url []

module Helper = struct
  let root_view ~js ~css page_data =
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
