open Dream_inertia
open Lwt.Syntax

let run_with_middleware =
  let inertia =
    let root_view = Inertia.Helper.root_view ~js:"" ~css:"" in
    Inertia.middleware ~version:"1" ~root_view ()
  in
  let session = Dream.memory_sessions in
  fun handler : Dream.handler -> session (inertia handler)

let render_page page req =
  let page_handler req = Inertia.render req page in
  run_with_middleware page_handler req

let html_response _switch () =
  let page = Inertia.page ~component:"test-componenent" ~url:"/home" () in
  let req = Dream.request ~target:"/home" "" in
  let+ resp = render_page page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  let mime = Dream.header resp "Content-Type" in
  let inertia_header = Dream.header resp "X-Inertia" in
  Alcotest.(check int) "Responded OK" 200 status_code;
  Alcotest.(check (option string))
    "First page are HTML" (Some Dream.text_html) mime;
  Alcotest.(check (option string))
    "X-Inertia header is set to true" (Some "true") inertia_header

let inertia_response _switch () =
  let page = Inertia.page ~component:"test-componenent" ~url:"/home" () in
  let req =
    Dream.request
      ~headers:[ ("X-Inertia", "true"); ("X-Inertia-Version", "1") ]
      ~target:"/home" ""
  in
  let+ resp = render_page page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  let mime = Dream.header resp "Content-Type" in
  let inertia_header = Dream.header resp "X-Inertia" in
  Alcotest.(check int) "Responded OK" 200 status_code;
  Alcotest.(check (option string))
    "First page are HTML" (Some Dream.application_json) mime;
  Alcotest.(check (option string))
    "X-Inertia header is set to true" (Some "true") inertia_header

let version_conflict _switch () =
  let page = Inertia.page ~component:"test-componenent" ~url:"/home" () in
  let req =
    Dream.request
      ~headers:[ ("X-Inertia", "true"); ("X-Inertia-Version", "2") ]
      ~target:"/home" ""
  in
  let+ resp = render_page page @@ req in
  let status_code = Dream.status resp |> Dream.status_to_int in
  let mime = Dream.header resp "X-Inertia-Location" in
  Alcotest.(check int) "Responded OK" 409 status_code;
  Alcotest.(check (option string)) "First page are HTML" (Some "/home") mime

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Inertia protocol"
       [
         ( "responses",
           [
             Alcotest_lwt.test_case "HTML responses" `Quick html_response;
             Alcotest_lwt.test_case "Inertia responses" `Quick inertia_response;
             Alcotest_lwt.test_case "Assets versionning" `Quick version_conflict;
           ] );
       ]
