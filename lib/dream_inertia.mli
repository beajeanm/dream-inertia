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

(** An inertia page oject *)
type page

(** The inertia instance *)
type inertia

(** The inertia equivalent of a {!Dream.handler}. *)
type handler = Dream.request -> page Lwt.t

(** The inertia equivalent of a {!Dream.route}. *)
type route

val create_page :
     ?props:Yojson.Safe.t
  -> ?status:Dream.status option
  -> ?headers:(string * string) list
  -> string
  -> page
(** Create a new page.

    {[ create_page ~props:(`Assoc [("name", `String "bob")]) "MyComponent" ]}

    Creates a page for the component MyComponent with the properties
    [{"name":"bob"}].*)

val init :
  version:string option -> template:(Yojson.Safe.t -> string) -> unit -> inertia
(** Initializes inertia.

    Template is a function generating the full HTML reponse, it receives the
    JSON object to insert in the data-app attribute of your application [<div>].

    {[ init ~template ]} *)

val router : inertia -> route list -> Dream.middleware
(** [router inertia routes] convert a list of inertia {!route} in a middleware
    equivalent to a {!Dream.router}. *)

val get : string -> handler -> route
(** Forwards [`GET] requests for the given path to the handler.

    {[ Dream.get "/home" home_template ]} *)

val head : string -> handler -> route
(** Forwards [`HEAD] requests for the given path to the handler.

    {[ Dream.head "/home" home_template ]} *)

val post : string -> handler -> route
(** Forwards [`POST] requests for the given path to the handler.

    {[ Dream.post "/home" home_template ]} *)

val put : string -> handler -> route
(** Forwards [`PUT] requests for the given path to the handler.

    {[ Dream.put "/home" home_template ]} *)

val delete : string -> handler -> route
(** Forwards [`DELETE] requests for the given path to the handler.

    {[ Dream.delete "/home" home_template ]} *)

val connect : string -> handler -> route
(** Forwards [`CONNECT] requests for the given path to the handler.

    {[ Dream.connect "/home" home_template ]} *)

val options : string -> handler -> route
(** Forwards [`OPTIONS] requests for the given path to the handler.

    {[ Dream.options "/home" home_template ]} *)

val trace : string -> handler -> route
(** Forwards [`TRACE] requests for the given path to the handler.

    {[ Dream.trace "/home" home_template ]} *)

val patch : string -> handler -> route
(** Forwards [`PATCH] requests for the given path to the handler.

    {[ Dream.patch "/home" home_template ]} *)

val inertia_versionning : inertia -> Dream.middleware
(** Middleware to manage assets versioning. See
    {{:https://inertiajs.com/asset-versioning} Asset versioning}*)

val error_template :
     inertia
  -> string
  -> Dream.error
  -> string option
  -> Dream.response
  -> Dream.response Dream.promise
(** Error template compatible with inertia
    {{:https://inertiajs.com/error-handling} Error Handling}. *)
