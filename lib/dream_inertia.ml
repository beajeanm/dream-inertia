type data_page = {component: string; props: Yojson.Safe.t}

type t =
  { template: Html_types.div Tyxml_html.elt -> Html_types.html Tyxml_html.elt
  ; version: string option }

let init template = {template; version= None}

let data_page_to_json data_page path version =
  `Assoc
    [ ("component", `String data_page.component)
    ; ("props", data_page.props)
    ; ("url", `String path)
    ; ("version", `String version) ]
  |> Yojson.Safe.to_string

let get_version = function Some v -> v | None -> "1"

let render_html {template; version} data_page path =
  let page_content =
    Format.asprintf {| <div id="app" data-page='%s'></div> |}
    @@ data_page_to_json data_page path (get_version version)
  in
  let app_div = Tyxml_html.Unsafe.data page_content in
  let html = template app_div in
  Dream.html @@ Format.asprintf "%a" (Tyxml_html.pp ()) html

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
