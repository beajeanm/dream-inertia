let is_inertia_request request =
  Dream.header request "X-Inertia" |> Option.is_some

let request_path request =
  begin
    Dream.to_path (Dream.path request)
  end
  [@alert "-deprecated"]

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
  let is_redirect response =
    let status = Dream.status response in
    status = `Moved_Permanently || status = `Found
  in
  let meth = Dream.method_ request in
  is_redirect response && (meth = `PUT || meth = `PATCH || meth = `DELETE)

let update_status (response : Dream.response) =
  let open Lwt.Syntax in
  let* body = Dream.body response in
  Dream.respond ~status:`See_Other ~headers:(Dream.all_headers response) body

let process_response request inner_handler =
  let open Lwt.Syntax in
  let set_location request response =
    if Dream.is_redirection (Dream.status response) then
      Dream.set_header response "Location" (request_path request)
    else ()
  in
  let* response = inner_handler request in
  set_location request response;
  set_csrf_cookie request response;
  if is_non_post_redirect request response then update_status response
  else Lwt.return response

let stale_response request =
  Dream.empty
    ~headers:[ ("X-Inertia-Location", request_path request) ]
    `Conflict

let create version inner_handler request =
  let request_version =
    Dream.header request "X-Inertia-Version" |> Option.value ~default:""
  in
  match
    ( Dream.method_ request,
      is_inertia_request request,
      String.equal request_version version )
  with
  | `GET, true, false -> stale_response request
  | _, _, _ -> process_response request inner_handler
