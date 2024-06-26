open Dream_inertia
open Inertia
open Example_lib

module Model = struct
  type user = { first_name : string; last_name : string; email : string }

  let user_to_json user =
    `Assoc
      [
        ("first_name", `String user.first_name);
        ("last_name", `String user.last_name);
        ("email", `String user.email);
      ]

  let json_to_user json =
    let first_name = Yojson.Safe.Util.(member "first_name" json |> to_string) in
    let last_name = Yojson.Safe.Util.(member "last_name" json |> to_string) in
    let email = Yojson.Safe.Util.(member "email" json |> to_string) in
    { first_name; last_name; email }

  let users : user list ref =
    ref
      [
        {
          first_name = "John";
          last_name = "Smith";
          email = "jonh.smith@test.com";
        };
      ]

  let add_user user = users := user :: !users
  let counter = ref 0
end

module Controller = struct
  open Model

  let home_page () =
    let generate_message () =
      Dream.log "Generating a message!!!!";
      `String "🐫 You've clicked: "
    in
    page ~component:"Home"
      ~props:
        Inertia.
          [
            ("counter", prop (`Int !counter));
            ("message", delayed_prop generate_message);
          ]
      ~url:"/" ()

  let count () =
    incr counter;
    home_page ()

  let about_page = page ~component:"About" ~url:"/about" ()

  let users_page () =
    let users_json = `List (List.map user_to_json !users) in
    page ~component:"Users"
      ~props:Inertia.[ ("users", prop users_json) ]
      ~url:"/users" ()

  let add_user inertia request =
    let open Lwt.Syntax in
    let csrf_token = Dream.header request "X-Xsrf-Token" |> Option.get in
    let cookie_token =
      Dream.cookie ~decrypt:false ~secure:false request "XSRF-TOKEN"
      |> Option.get
    in
    let* csrf_result = Dream.verify_csrf_token request csrf_token in
    match (csrf_result, String.equal cookie_token csrf_token) with
    | `Ok, true ->
        let* json_body = Dream.body request in
        let user = json_to_user (Yojson.Safe.from_string json_body) in

        if String.equal user.first_name "John" then
          Inertia.add_flash_message request
            (`Assoc [ ("first_name", `String "first name should not be John") ])
        else Model.add_user user;

        Inertia.inertia_response inertia ~status:`Found request
          (page ~component:"Users" ~url:"/users" ())
    | __ -> Dream.empty `Bad_Request
end

let routes inertia =
  let open Controller in
  [
    Dream.get "/" (fun req ->
        Inertia.inertia_response inertia req @@ home_page ());
    Dream.get "/about" (fun req ->
        Inertia.inertia_response inertia req about_page);
    Dream.get "/count" (fun req ->
        Inertia.inertia_response inertia req @@ count ());
    Dream.get "/users" (fun req ->
        Inertia.inertia_response inertia req @@ users_page ());
    Dream.get "/error" (fun _ -> failwith "This is a server side error!");
    Dream.post "/users" (add_user inertia);
    Dream.get "/favicon.ico" (Loader.asset_loader "" "favicon.ico");
    Dream.get "/robots.txt" (Loader.asset_loader "" "robots.txt");
    Dream.get "/assets/**" @@ Dream.static ~loader:Loader.asset_loader "assets/";
  ]

let parse_manifest () =
  Assets.read "manifest.json"
  |> Option.map Yojson.Safe.from_string
  |> Option.get

let find_js manifest =
  let open Yojson.Safe.Util in
  member "src/js/main.js" manifest |> member "file" |> to_string

let find_css manifest =
  let open Yojson.Safe.Util in
  member "src/js/main.js" manifest
  |> member "css" |> to_list |> List.hd |> to_string

let parse_version manifest =
  let js = find_js manifest in
  let prefix_length = String.length "assets/main-" in
  String.sub js prefix_length (String.length js - prefix_length - 3)

let () =
  let manifest = parse_manifest () in
  let js = find_js manifest in
  let css = find_css manifest in
  let version = parse_version manifest in
  let root_view = Inertia.Helper.root_view ~js ~css in
  let inertia = Inertia.init ~version ~root_view () in

  Dream.run
    ~error_handler:
      (Inertia.Helper.error_handler inertia Dream.debug_error_handler)
  @@ Dream.logger @@ Inertia.middleware inertia @@ Dream.origin_referrer_check
  @@ Dream.memory_sessions @@ Dream.flash
  @@ Dream.router (routes inertia)
