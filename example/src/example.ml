let inertia =
  Dream_inertia.init
    ~base_url:Uri.(of_string "http://localhost:8080")
    ~version:None ~template:Index.render ()

let () =
  let content = Yojson.Safe.from_string {json|
  { "name" : "world"}
  |json} in
  let data = Dream_inertia.{component= "Home"; props= content} in
  let handler = Dream_inertia.inertia_handler inertia (fun _req -> data) in
  Dream.run ~adjust_terminal:false
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" handler
       ; Dream.get "/favicon.png"
         @@ Dream.from_filesystem "public" "favicon.png"
       ; Dream.get "/build/**" @@ Dream.static "public/build/" ]
  @@ Dream.not_found
