type data_page = {component: string; props: Yojson.Safe.t}

type t

val init :
     version:string option
  -> template:(Yojson.Safe.t -> string)
  -> unit
  -> t

val inertia_handler : t -> (Dream.request -> data_page) -> Dream.handler

val inertia_versionning : t -> Dream.middleware
