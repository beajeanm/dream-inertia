let inertia =
  Dream_inertia.init
    ~base_url:Uri.(of_string "http://localhost:8080")
    ~version:None ~template:Index.render ()

let crunch_build_loader _directory path _request =
  match Assets.Build.read path with
  | Some data ->
      Dream.respond data
  | None ->
      Dream.empty `Not_Found

let crunch_static_loader _directory path _request =
  match Assets.Static.read path with
  | Some data ->
      Dream.respond data
  | None ->
      Dream.empty `Not_Found

let () =
  let _ = Assets.Build.file_list in
  let content = Yojson.Safe.from_string {json|
  { "name" : "world"}
  |json} in
  let data = Dream_inertia.{component= "Home"; props= content} in
  let intertia_handler =
    Dream_inertia.inertia_handler inertia (fun _req -> data)
  in
  Dream.run ~adjust_terminal:false
  @@ Dream.logger
  @@ Dream_livereload.(
       inject_script
       (* We are running webpack, then crunch before the ocaml build, so we need
          a bit more time to reload *)
         ~script:(default_script ~retry_interval_ms:1000 ~max_retry_ms:10000 ())
         ())
  @@ Dream.router
       [ Dream.get "/" intertia_handler
       ; Dream_livereload.route ()
       ; Dream.get "/favicon.png"
         @@ Dream.from_filesystem "public" "favicon.png"
       ; Dream.get "/build/**" @@ Dream.static ~loader:crunch_build_loader ""
       ; Dream.get "/static/**" @@ Dream.static ~loader:crunch_static_loader ""
       ]
  @@ Dream.not_found
