type data_page

val create_page :
     ?props:Yojson.Safe.t
  -> ?status:Dream.status option
  -> ?code:int option
  -> ?headers:(string * string) list
  -> string
  -> data_page

type t

val init :
  version:string option -> template:(Yojson.Safe.t -> string) -> unit -> t

val inertia_handler : t -> (Dream.request -> data_page) -> Dream.handler

val inertia_versionning : t -> Dream.middleware

val error_template :
  t -> string -> Dream.error -> string option -> Dream.response -> Dream.response Dream.promise
