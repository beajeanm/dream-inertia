(******************************************************************************)
(* MIT License                                                                *)
(*                                                                            *)
(* Copyright (c) 2021 Jean-Michel Bea                                         *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining a    *)
(* copy of this software and associated documentation files (the "Software"), *)
(* to deal in the Software without restriction, including without limitation  *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *)
(* and/or sell copies of the Software, and to permit persons to whom the      *)
(* Software is furnished to do so, subject to the following conditions:       *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software.                        *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *)
(* DEALINGS IN THE SOFTWARE.                                                  *)
(******************************************************************************)

open Base

let inertia = Dream_inertia.init ~version:None ~template:Index.render ()

let full_page_request _ () =
  let request = Dream.request ~method_:`GET ~target:"/" "" in
  let%lwt response =
    let open Dream_inertia in
    let inertia_middleware =
      Dream.router
      @@ [ route_to_dream inertia
             (get "/" @@ fun _req -> Lwt.return @@ create_page "Test") ]
    in
    inertia_middleware Dream.not_found request
  in
  let content_type =
    Option.value ~default:"No content type"
    @@ Dream.header "Content-Type" response
  in
  Lwt.return
  @@ Alcotest.(check string "Full app page" Dream.text_html content_type)

let incremental_page_request _ () =
  let request =
    Dream.request ~method_:`GET ~target:"/" ~headers:[("X-Inertia", "true")] ""
  in
  let%lwt response =
    let open Dream_inertia in
    let inertia_middleware =
      Dream.router
      @@ [ route_to_dream inertia
             (get "/" @@ fun _req -> Lwt.return @@ create_page "Test") ]
    in
    inertia_middleware Dream.not_found request
  in
  let content_type =
    Option.value ~default:"No content type"
    @@ Dream.header "Content-Type" response
  in
  let inertia_flag =
    Option.value ~default:"No inertia flag" @@ Dream.header "X-Inertia" response
  in
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
  let path =
    Option.value ~default:"No inertia location"
    @@ Dream.header "X-Inertia-Location" response
  in
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

let get_errors request = Dream.flash request

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
