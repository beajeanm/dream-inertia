type data_page =
  { component: string
  ; props: Yojson.Safe.t
  ; status: Dream.status option
  ; code: int option
  ; headers: (string * string) list }

let create_page ?(props = `Assoc []) ?(status = None) ?(code = None)
    ?(headers = []) component_name =
  {component= component_name; props; status; code; headers}

let add_header data_page name value =
  let headers = List.concat [[(name, value)]; data_page.headers] in
  {data_page with headers}

type t = {template: Yojson.Safe.t -> string; version: string option}

let init ~version ~template () = {template; version}

let data_page_to_json data_page path version =
  `Assoc
    [ ("component", `String data_page.component)
    ; ("props", data_page.props)
    ; ("url", `String path)
    ; ("version", `String version) ]

let get_version inertia = Option.value inertia.version ~default:"1"

let render_html inertia data_page path =
  let data_page = add_header data_page "X-Inertia" "true" in
  let status = Option.value data_page.status ~default:`OK in
  Dream.html ~headers:data_page.headers ~status
  @@ inertia.template
  @@ data_page_to_json data_page path (get_version inertia)

let render_json inertia data_page path =
  let data_page = add_header data_page "X-Inertia" "true" in
  let status = Option.value data_page.status ~default:`OK in
  Dream.json ~headers:data_page.headers ~status
  @@ Yojson.Safe.to_string
  @@ data_page_to_json data_page path (get_version inertia)

let inertia_handler t fn request =
  let data = fn request in
  let path = "/" ^ String.concat "/" @@ Dream.path request in
  match Dream.header "X-Inertia" request with
  | Some "true" ->
      render_json t data path
  | _ ->
      render_html t data path

let inertia_versionning t handler =
  let current_vesion = get_version t in
  fun request ->
    let request_version = Dream.header "X-Inertia-Version" request in
    let target = Dream.target request in
    match (Dream.method_ request, request_version) with
    | `GET, Some v ->
        if String.equal v current_vesion then handler request
        else Dream.empty ~headers:[("X-Inertia-Location", target)] `Conflict
    | _ ->
        handler request

let error_template t component debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status in
  let is_json =
    Option.map (String.equal Dream.application_json)
    @@ Dream.header "Content-type" suggested_response
  in
  let debug_info = match debug_info with Some x -> x | _ -> "" in
  let data_page =
    create_page ~status:(Some status)
      ~props:(`Assoc [("status", `Int code); ("debug_info", `String debug_info)])
      component
  in
  match is_json with
  | Some true ->
      render_json t data_page "/error"
  | _ ->
      render_html t data_page "/"
