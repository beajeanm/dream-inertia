let inertia = Dream_inertia.init ~version:None ~template:Index.render ()

let full_page_request _ () =
  let request = Dream.request ~method_:`GET ~target:"/" "" in
  let%lwt response =
    Dream_inertia.inertia_handler inertia
      (fun _req -> {component= "Test"; props= `Assoc []})
      request
  in
  let content_type = Option.get @@ Dream.header "Content-Type" response in
  Lwt.return
  @@ Alcotest.(check string "Full app page" Dream.text_html content_type)

let incremental_page_request _ () =
  let request =
    Dream.request ~method_:`GET ~target:"/" ~headers:[("X-Inertia", "true")] ""
  in
  let%lwt response =
    Dream_inertia.inertia_handler inertia
      (fun _req -> {component= "Test"; props= `Assoc []})
      request
  in
  let content_type = Option.get @@ Dream.header "Content-Type" response in
  let inertia_flag = Option.get @@ Dream.header "X-Inertia" response in
  Lwt.return
  @@ Alcotest.(
       check (pair string string) "Full app page"
         (Dream.application_json, "true")
         (content_type, inertia_flag))

let matching_version _ () =
  let request =
    Dream.request ~method_:`GET ~target:"/"
      ~headers:[("X-Inertia-Version", "1")]
      ""
  in
  let%lwt response =
    Dream_inertia.inertia_versionning inertia
      (fun _req -> Dream.empty `OK)
      request
  in
  let satus = Dream.status_to_int @@ Dream.status response in
  Lwt.return
  @@ Alcotest.(check int "Regular answer" (Dream.status_to_int `OK) satus)

let version_update _ () =
  let request =
    Dream.request ~method_:`GET ~target:"/mytarget"
      ~headers:[("X-Inertia-Version", "older one")]
      ""
  in
  let%lwt response =
    Dream_inertia.inertia_versionning inertia
      (fun _req -> Dream.empty `OK)
      request
  in
  let satus = Dream.status_to_int @@ Dream.status response in
  let path = Option.get @@ Dream.header "X-Inertia-Location" response in
  Lwt.return
  @@ Alcotest.(
       check (pair int string) "Conflict redirect"
         (Dream.status_to_int `Conflict, "/mytarget")
         (satus, path))

let only_redirect_get _ () =
  let request =
    Dream.request ~method_:`POST ~target:"/mytarget"
      ~headers:[("X-Inertia-Version", "older one")]
      ""
  in
  let%lwt response =
    Dream_inertia.inertia_versionning inertia
      (fun _req -> Dream.empty `OK)
      request
  in
  let satus = Dream.status_to_int @@ Dream.status response in
  Lwt.return
  @@ Alcotest.(check int "Don't redirect post" (Dream.status_to_int `OK) satus)

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "inertia"
       [ ( "Basic flow"
         , [ test_case "Full page request" `Quick full_page_request
           ; test_case "Incremental page request" `Quick
               incremental_page_request ] )
       ; ( "Versionning"
         , [ test_case "Same version" `Quick matching_version
           ; test_case "New version" `Quick version_update
           ; test_case "POST no-versioning" `Quick only_redirect_get ] ) ]
