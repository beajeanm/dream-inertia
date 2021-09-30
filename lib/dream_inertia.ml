open Base

let log = Dream.sub_log "inertia"

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

let get_version t = Option.value t.version ~default:"1"

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

let render t request data_page =
  let path = String.concat ~sep:"/" @@ List.concat [[""]; Dream.path request] in
  let version = get_version t in
  let errors = get_errors request in
  let render_html () =
    let data_page = add_header data_page "X-Inertia" "true" in
    let status = Option.value data_page.status ~default:`OK in
    Dream.html ~headers:data_page.headers ~status
    @@ t.template
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

let inertia_handler t fn request =
  let data = fn request in
  render t request data

let inertia_versionning t handler =
  let current_vesion = get_version t in
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

let error_template t component (error : Dream.error) debug_info
    suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status in
  let debug_info = Option.value ~default:"" debug_info in
  let data_page =
    create_page ~status:(Some status)
      ~props:(`Assoc [("status", `Int code); ("debug_info", `String debug_info)])
      component
  in
  let inertia_response request = render t request data_page in
  Option.value ~default:(Dream.empty status)
  @@ Option.map ~f:inertia_response error.request
