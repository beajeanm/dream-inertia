type t
type page
type handler = Dream.request -> page Lwt.t

val make : version:string -> js_path:string -> css_path:string -> unit -> t
val middleware : t -> Dream.middleware

type prop

val prop : Yojson.Safe.t -> prop
val lazy_prop : (unit -> Yojson.Safe.t) -> prop
val closure_prop : (unit -> Yojson.Safe.t) -> prop
val always_prop : Yojson.Safe.t -> prop

val page :
  component:string -> ?props:(string * prop) list -> url:string -> unit -> page

val inertia_response :
  t ->
  ?headers:(string * string) list ->
  ?status:Dream.status ->
  (unit -> page) ->
  Dream.request ->
  Dream.response Lwt.t
