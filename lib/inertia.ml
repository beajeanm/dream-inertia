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
    merge_props : string list;
    prepend_props : string list;
    match_on : string list;
  }

  let with_prop page prop = { page with props = prop :: page.props }

  let with_defer_prop page defer =
    { page with deferred_props = defer :: page.deferred_props }
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

let page ~component ?(props = []) ?(deferred_props = []) ?(merge_props = [])
    ?(prepend_props = []) ?(match_on = []) ~url () =
  Page.
    {
      component;
      props;
      deferred_props;
      url;
      merge_props;
      prepend_props;
      match_on;
    }

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

let flash_key = "Inertia-errors"
let shared_data_key = "Inertia-shared-data"

let flash_errors request =
  let open Option.Infix in
  let prefix = flash_key ^ "." in
  List.filter_map
    (fun (key, value) ->
      let+ key = String.chop_prefix ~pre:prefix key in
      (key, value))
    (Dream.flash_messages request)
  |> List.Assoc.map_values Yojson.Safe.from_string

let add_error request key data =
  Dream.add_flash_message request
    (Format.asprintf "%s.%s" flash_key key)
    (Yojson.Safe.to_string data)

let shared_data request =
  Dream.session_field request shared_data_key
  |> Option.map Yojson.Safe.from_string
  |> Option.map Yojson.Safe.Util.to_assoc
  |> Option.value ~default:[]

let add_shared_data request key prop =
  let current_data = shared_data request in
  let updated = List.Assoc.set ~eq:String.equal key prop current_data in
  Dream.set_session_field request shared_data_key
    (Yojson.Safe.to_string (`Assoc updated))

let error_props_key = "errors"

let merge_data request page =
  let add_prop list (k, v) = List.Assoc.set ~eq:String.equal k v list in
  let props =
    add_prop page.Page.props (error_props_key, `Assoc (flash_errors request))
  in
  let shared_data = shared_data request in
  (* shared data will overwrite regular props. *)
  List.fold_left add_prop shared_data props

let full_page_load request version page =
  let open Page in
  let props = merge_data request page in
  let base_data =
    [
      ("component", `String page.component);
      ("props", `Assoc props);
      ("url", `String page.url);
      ("version", `String version);
    ]
  in
  let deferred_data =
    if List.is_empty page.deferred_props then None
    else
      let groupped_props =
        List.group_by
          ~hash:(fun dp -> String.hash dp.Page.group)
          ~eq:(fun dp1 dp2 -> String.equal dp1.Page.group dp2.Page.group)
          page.deferred_props
      in
      let props_keys =
        List.map
          (fun props ->
            let group_name = (List.hd props).Page.group in
            let keys = List.map Page.(fun dp -> `String dp.key) props in
            (group_name, `List keys))
          groupped_props
      in
      Some props_keys
  in
  let all_props =
    Option.map
      (fun deferred -> ("deferredProps", `Assoc deferred) :: base_data)
      deferred_data
    |> Option.value ~default:base_data
  in
  Yojson.Safe.to_string (`Assoc all_props)

let partial_page_load request version page component filter =
  let reset_props =
    Dream.header request "X-Inertia-Reset"
    |> Option.map (String.split_on_char ',')
    |> Option.value ~default:[]
  in
  let open Page in
  let filter_props = List.filter (fun (k, _) -> filter k) page.props in
  let filter_deferred =
    List.filter (fun dprop -> filter dprop.Page.key) page.deferred_props
    |> List.map (fun dprop -> Page.(dprop.key, dprop.callback ()))
  in
  (* defer props key will overwrite regular props *)
  let all_props =
    List.fold_left
      (fun props (key, value) ->
        List.Assoc.set ~eq:String.equal key value props)
      filter_props filter_deferred
  in
  let all_prop_keys = List.Assoc.keys all_props in
  let filter_names ?(eq = String.equal) property names =
    let filtered =
      List.filter_map
        (fun s ->
          if List.mem ~eq s all_prop_keys && not (List.mem ~eq s reset_props)
          then Some (`String s)
          else None)
        names
    in
    if List.is_empty filtered then [] else [ (property, `List filtered) ]
  in

  let base_data =
    [
      ("component", `String component);
      ("props", `Assoc all_props);
      ("url", `String page.url);
      ("version", `String version);
    ]
  in
  let merge_props = filter_names "mergeProps" page.merge_props in
  let prepend_props = filter_names "prependProps" page.prepend_props in
  let match_on =
    filter_names
      ~eq:(fun prefix -> String.starts_with ~prefix)
      "matchPropsOn" page.match_on
  in
  Yojson.Safe.to_string
    (`Assoc (List.concat [ base_data; merge_props; prepend_props; match_on ]))

let render ?(headers = []) ?(status = `OK) request page =
  let headers = ("Vary", "X-Inertia") :: ("X-Inertia", "true") :: headers in
  match classify_request request with
  | `Full_page ->
      Dream.html ~status ~headers
      @@ config.root_view (full_page_load request config.version page)
  | `Inertia ->
      Dream.json ~status ~headers @@ full_page_load request config.version page
  | `Partial_include (component, fields) ->
      Dream.json ~status ~headers
      @@ partial_page_load request config.version page component
           (Fun.flip List.mem @@ fields)
  | `Partial_exclude (component, fields) ->
      Dream.json ~status ~headers
      @@ partial_page_load request config.version page component
           (Fun.negate @@ Fun.flip List.mem @@ fields)

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
