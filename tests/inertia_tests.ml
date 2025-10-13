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

type session_data = { session : string; token : string }

let extract_cookie name response =
  let cookies =
    Dream.headers response "set-cookie"
    |> List.filter (String.starts_with ~prefix:name)
  in
  match cookies with
  | cookie :: _ ->
      String.split_on_char ';' cookie
      |> List.hd |> String.split_on_char '=' |> List.tl |> List.hd
  | _ -> ""

let session_data () =
  let cookie_req = Dream.request ~target:"/home" "" in
  set_headers cookie_req;
  let+ cookie_resp = render_page default_page cookie_req in
  {
    session = extract_cookie "dream.session" cookie_resp;
    token = extract_cookie "XSRF-TOKEN" cookie_resp;
  }

let set_session req session =
  Dream.set_header req "Cookie"
    (Format.asprintf "%s=%s" "dream.session" session)

let invalid_xsrf_token _switch () =
  let* data = session_data () in
  let req = Dream.request ~target:"/home" "" ~method_:`POST in
  set_headers req;
  Dream.set_header req "X-Xsrf-Token" "Not a valid token";
  set_session req data.session;
  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  Alcotest.(check int) "Responded Bad Response" 400 status_code

let valid_xsrf _switch () =
  let* data = session_data () in
  let req = Dream.request ~target:"/home" "" ~method_:`POST in
  set_headers req;
  Dream.set_header req "X-Xsrf-Token" data.token;
  set_session req data.session;
  let+ resp = render_page default_page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  Alcotest.(check int) "Responded Ok" 200 status_code

let shared_data _switch () =
  let inertia = create_inertia () in
  let key = "test-shared-key" in
  let data = `Assoc [ ("test-shared-data", `String "test-value") ] in
  let handler req =
    let* () = Inertia.add_shared_data req key data in
    let page_handler req = Inertia.render req default_page in
    run_with_middleware ~inertia page_handler req
  in
  let* session_data = session_data () in
  let req = Dream.request ~target:"/home" "" in
  set_headers req;
  set_session req session_data.session;
  let* resp = run_with_middleware handler req in
  let+ body = Dream.body resp in
  let json_body = Yojson.Safe.from_string body in
  let shared_props =
    Yojson.Safe.Util.member "props" json_body
    |> Yojson.Safe.Util.to_assoc |> List.assoc key
  in
  Alcotest.(check string)
    "Received shared data"
    (Yojson.Safe.to_string data)
    (Yojson.Safe.to_string shared_props)

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
             test "Invalid XSRF token" invalid_xsrf_token;
             test "Valid XSRF" valid_xsrf;
           ] );
         ("Props", [ test "Shared data" shared_data ]);
       ]
