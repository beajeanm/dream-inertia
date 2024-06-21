type t
type page
type handler = Dream.request -> page Lwt.t

val make : version:string -> js_path:string -> css_path:string -> unit -> t

val page :
  component:string ->
  ?props:(string * Yojson.Safe.t) list ->
  ?lazy_props:(string * Yojson.Safe.t Lazy.t) list ->
  url:string ->
  ?headers:(string * string) list ->
  ?status:Dream.status ->
  unit ->
  page

val get : t -> string -> handler -> Dream.route
val post : t -> string -> handler -> Dream.route
val put : t -> string -> handler -> Dream.route
val delete : t -> string -> handler -> Dream.route
