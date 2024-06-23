type t
type page
type handler = Dream.request -> page Lwt.t

val make : version:string -> js_path:string -> css_path:string -> unit -> t
val middleware : t -> Dream.middleware

val page :
  component:string ->
  ?props:(string * Yojson.Safe.t) list ->
  ?lazy_props:(string * Yojson.Safe.t Lazy.t) list ->
  url:string ->
  unit ->
  page

val inertia_response :
  t ->
  ?headers:(string * string) list ->
  ?status:Dream.status ->
  (unit -> page) ->
  Dream.request ->
  Dream.response Lwt.t
