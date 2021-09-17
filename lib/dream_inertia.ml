type data_page = {component: string; props: Yojson.Safe.t}

type t = {template: string -> string; base_url: Uri.t; version: string option}

let init ~version ~base_url ~template () = {template; base_url; version}

let data_page_to_json data_page path version =
  `Assoc
    [ ("component", `String data_page.component)
    ; ("props", data_page.props)
    ; ("url", `String path)
    ; ("version", `String version) ]
  |> Yojson.Safe.to_string

let get_version = function Some v -> v | None -> "1"

let render_html inertia data_page path =
  let page_content =
    Format.asprintf {| <div id="app" data-page='%s'></div> |}
    @@ data_page_to_json data_page path (get_version inertia.version)
  in
  Dream.html @@ inertia.template page_content

let render_json data_page path version =
  Dream.json @@ data_page_to_json data_page path (get_version version)

let inertia_handler t fn request =
  let data = fn request in
  let path = "/" ^ String.concat "/" @@ Dream.path request in
  match Dream.header "X-Inertia" request with
  | Some "true" ->
      render_json data path t.version
  | _ ->
      render_html t data path

let inertia_versionning t handler =
  let current_vesion = get_version t.version in
  fun request ->
    let request_version = Dream.header "X-Inertia-Version" request in
    let target = Dream.target request in
    let location =
      let updated_path = Uri.path t.base_url ^ target in
      Uri.to_string @@ Uri.with_path t.base_url updated_path
    in
    if
      Option.is_some request_version
      && not (String.equal current_vesion (Option.get request_version))
    then Dream.empty ~headers:[("X-Inertia-Location", location)] `Conflict
    else handler request
