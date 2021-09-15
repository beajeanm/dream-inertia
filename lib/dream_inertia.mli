type data_page = {component: string; props: Yojson.Safe.t}

type t

val init :
  (Html_types.div Tyxml_html.elt -> Html_types.html Tyxml_html.elt) -> t

val inertia_handler : t -> (Dream.request -> data_page) -> Dream.handler
