let inertia = Dream_inertia.init ~version:None ~template:Index.render ()

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
  let props = Yojson.Safe.from_string {json|
  { "name" : "world"}
  |json} in
  let data = Dream_inertia.(create_page ~props "Home") in
  let inertia_router =
    Dream_inertia.router inertia
      [(Dream_inertia.get "/" @@ fun _req -> Lwt.return data)]
  in
  Dream.run ~adjust_terminal:false
  @@ Dream.logger @@ inertia_router
  @@ Dream.router
       [ Dream.get "/favicon.png" @@ Dream.from_filesystem "public" "favicon.png"
       ; Dream.get "/build/**" @@ Dream.static ~loader:crunch_build_loader ""
       ; Dream.get "/static/**" @@ Dream.static ~loader:crunch_static_loader ""
       ]
  @@ Dream.not_found
