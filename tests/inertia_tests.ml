open Dream_inertia
open Lwt.Syntax

let create_inertia () =
  let root_view = Inertia.Helper.root_view ~js:"" ~css:"" in
  Inertia.middleware ~version:"1" ~root_view ()

module State = struct
  (* memory session kept state in an hash table and inertia is bound to the session state. *)
  let inertia = create_inertia ()
  let session = Dream.memory_sessions ~lifetime:(60. *. 60.)
end

let run_with_middleware ?(inertia = State.inertia) ?(session = State.session) =
 fun handler : Dream.handler -> session (inertia handler)

let default_page = Inertia.page ~component:"test-componenent" ~url:"/home" ()

let set_headers req =
  Dream.set_header req "X-Inertia" "true";
  Dream.set_header req "X-Inertia-Version" "1"

let render_page page req =
  let page_handler req = Inertia.render req page in
  run_with_middleware page_handler req

let html_response _switch () =
  let req = Dream.request ~target:"/home" "" in
  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  let mime = Dream.header resp "Content-Type" in
  let inertia_header = Dream.header resp "X-Inertia" in
  Alcotest.(check int) "Responded OK" 200 status_code;
  Alcotest.(check (option string))
    "First page are HTML" (Some Dream.text_html) mime;
  Alcotest.(check (option string))
    "X-Inertia header is set to true" (Some "true") inertia_header

let inertia_response _switch () =
  let req = Dream.request ~target:"/home" "" in
  set_headers req;
  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  let mime = Dream.header resp "Content-Type" in
  let inertia_header = Dream.header resp "X-Inertia" in
  Alcotest.(check int) "Responded OK" 200 status_code;
  Alcotest.(check (option string))
    "First page are HTML" (Some Dream.application_json) mime;
  Alcotest.(check (option string))
    "X-Inertia header is set to true" (Some "true") inertia_header

let version_conflict _switch () =
  let req =
    Dream.request
      ~headers:[ ("X-Inertia", "true"); ("X-Inertia-Version", "2") ]
      ~target:"/home" ""
  in
  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  let mime = Dream.header resp "X-Inertia-Location" in
  Alcotest.(check int) "Responded OK" 409 status_code;
  Alcotest.(check (option string)) "First page are HTML" (Some "/home") mime

let missing_xsrf_token _switch () =
  let req = Dream.request ~target:"/home" "" ~method_:`POST in
  set_headers req;
  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  Alcotest.(check int) "Responded Bad request" 400 status_code

let extract_session response =
  let token_cookie =
    Dream.headers response "set-cookie"
    |> List.filter (String.starts_with ~prefix:"dream.session")
    |> List.hd
  in
  String.split_on_char ';' token_cookie
  |> List.hd |> String.split_on_char '=' |> List.tl |> List.hd

let extract_xsrf_token response =
  let token_cookie =
    Dream.headers response "set-cookie"
    |> List.filter (String.starts_with ~prefix:"XSRF-TOKEN")
    |> List.hd
  in
  String.split_on_char ';' token_cookie
  |> List.hd |> String.split_on_char '=' |> List.tl |> List.hd

let missing_xsrf_cookie _switch () =
  let cookie_req = Dream.request ~target:"/home" "" in
  set_headers cookie_req;
  let* cookie_resp = render_page default_page cookie_req in
  let token = extract_xsrf_token cookie_resp in
  let req = Dream.request ~target:"/home" "" ~method_:`POST in
  set_headers req;
  Dream.set_header req "X-Xsrf-Token" token;
  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  Alcotest.(check int) "Responded Bad request" 400 status_code

let invalid_xsrf_token _switch () =
  let cookie_req = Dream.request ~target:"/home" "" in
  set_headers cookie_req;

  let* cookie_resp = render_page default_page cookie_req in

  let req = Dream.request ~target:"/home" "" ~method_:`POST in
  set_headers req;
  Dream.set_header req "X-Xsrf-Token" "Not a valid token";
  Dream.set_header req "Cookie"
    (Format.asprintf "%s=%s;%s=%s" "XSRF-TOKEN"
       (extract_xsrf_token cookie_resp)
       "dream.session"
       (extract_session cookie_resp));

  Dream.error (fun log -> log "%s" (extract_session cookie_resp));

  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  Alcotest.(check int) "Responded Bad Response" 400 status_code

let valid_xsrf _switch () =
  let cookie_req = Dream.request ~target:"/home" "" in
  set_headers cookie_req;

  let* cookie_resp = render_page default_page cookie_req in

  let req = Dream.request ~target:"/home" "" ~method_:`POST in
  set_headers req;
  Dream.set_header req "X-Xsrf-Token" (extract_xsrf_token cookie_resp);
  Dream.set_header req "Cookie"
    (Format.asprintf "%s=%s;%s=%s" "XSRF-TOKEN"
       (extract_xsrf_token cookie_resp)
       "dream.session"
       (extract_session cookie_resp));

  Dream.error (fun log -> log "%s" (extract_session cookie_resp));

  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  Alcotest.(check int) "Responded Ok" 200 status_code

let () =
  let test msg f = Alcotest_lwt.test_case msg `Quick f in
  Lwt_main.run
  @@ Alcotest_lwt.run "Inertia protocol"
       [
         ( "responses",
           [
             test "HTML responses" html_response;
             test "Inertia responses" inertia_response;
             test "Assets versionning" version_conflict;
           ] );
         ( "CSRF",
           [
             test "Missing XSRF token" missing_xsrf_token;
             test "Missing XSRF cookie" missing_xsrf_cookie;
             test "Invalid XSRF token" invalid_xsrf_token;
             test "Valid XSRF" valid_xsrf;
           ] );
       ]
