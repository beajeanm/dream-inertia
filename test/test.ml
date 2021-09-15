let inertia =
  Dream_inertia.init (fun app_div ->
      Tyxml_html.(html (head (title (txt "Test")) []) (body [app_div])) )

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

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "inertia"
       [ ( "inertia"
         , [ test_case "Full page request" `Quick full_page_request
           ; test_case "Incremental page request" `Quick
               incremental_page_request ] ) ]
