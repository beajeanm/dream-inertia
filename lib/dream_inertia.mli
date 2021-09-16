type data_page = {component: string; props: Yojson.Safe.t}

type t

val init :
     version:string option
  -> base_uri:string
  -> template:(Html_types.div Tyxml_html.elt -> Html_types.html Tyxml_html.elt)
  -> unit
  -> t

val inertia_handler : t -> (Dream.request -> data_page) -> Dream.handler

val inertia_versionning : t -> Dream.middleware
