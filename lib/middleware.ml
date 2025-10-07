let log = Dream.sub_log "inertiajs"

let is_inertia_request request =
  Dream.header request "X-Inertia" |> Option.is_some

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

let check_csrf request =
  let check_csrf request =
    let open Lwt.Syntax in
    let header_token = Dream.header request "X-Xsrf-Token" |> Option.get in
    let cookie_token =
      Dream.cookie ~decrypt:false ~secure:false request "XSRF-TOKEN"
      |> Option.value ~default:""
    in
    let+ check_result = Dream.verify_csrf_token request header_token in
    match (check_result, String.equal cookie_token header_token) with
    | `Ok, true -> Ok ()
    | _ -> Error "Inavlid token"
  in
  match Dream.method_ request with
  (* Safe methods *)
  | `OPTIONS | `GET | `TRACE | `HEAD -> Lwt_result.return ()
  (* Dream.method_ should have normalized the method. *)
  | `Method _ -> Lwt_result.fail "Unexpected method"
  (* We are not a proxy, not sure why we would need this. *)
  | `CONNECT -> Lwt_result.fail "Unsupported method: connect"
  (* Check for CSRF token for everything else*)
  | `DELETE | `PUT | `POST | `PATCH -> check_csrf request

let process_response request inner_handler =
  let open Lwt.Syntax in
  let set_location request response =
    if Dream.is_redirection (Dream.status response) then
      Dream.set_header response "Location" (Dream.target request)
    else ()
  in
  (* Check csrf token before doing any processing. *)
  let* csrf_check = check_csrf request in
  match csrf_check with
  | Ok () ->
      let* response = inner_handler request in
      set_location request response;
      (* Refreshing the csrf token on every request sounds a bit excessive, but it will do for now.*)
      set_csrf_cookie request response;
      if is_non_post_redirect request response then update_status response
      else Lwt.return response
  | Error _ ->
      log.error (fun log -> log "Invalid csrf token.");
      Dream.empty `Bad_Request

let stale_response request =
  log.info (fun log -> log "Stale version detected");
  Dream.empty
    ~headers:[ ("X-Inertia-Location", Dream.target request) ]
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
