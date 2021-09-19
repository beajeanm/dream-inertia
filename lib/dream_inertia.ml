type data_page = {component: string; props: Yojson.Safe.t}

type t =
  {template: Yojson.Safe.t -> string; base_url: Uri.t; version: string option}

let init ~version ~base_url ~template () = {template; base_url; version}

let data_page_to_json data_page path version =
  `Assoc
    [ ("component", `String data_page.component)
    ; ("props", data_page.props)
    ; ("url", `String path)
    ; ("version", `String version) ]

let get_version = function Some v -> v | None -> "1"

let render_html inertia data_page path =
  Dream.html @@ inertia.template
  @@ data_page_to_json data_page path (get_version inertia.version)

let render_json data_page path version =
  Dream.json @@ Yojson.Safe.to_string
  @@ data_page_to_json data_page path (get_version version)

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
    match (Dream.method_ request, request_version) with
    | `GET, Some v ->
        if String.equal v current_vesion then handler request
        else Dream.empty ~headers:[("X-Inertia-Location", location)] `Conflict
    | _ ->
        handler request
