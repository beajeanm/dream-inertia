open Dream_inertia

let routes =
  let open Inertia in
  let inertia =
    make ~version:Loader.version ~js_path:Loader.index_js ~css_path:Loader.index_css ()
  in
  let home = page ~component:"Home" ~props:(`Assoc []) ~url:"/" () in
  let about = page ~component:"About" ~props:(`Assoc []) ~url:"/" () in
  [
    Inertia.get inertia "/" (fun _ -> Lwt.return home);
    Inertia.get inertia "/about" (fun _ -> Lwt.return about);
    Dream.get "/favicon.ico" (Loader.asset_loader "" "favicon.ico");
    Dream.get "/robots.txt" (Loader.asset_loader "" "robots.txt");
    Dream.get "/assets/**" @@ Dream.static ~loader:Loader.asset_loader "assets/";
  ]

let () = Dream.run @@ Dream.logger @@ Dream.router routes
