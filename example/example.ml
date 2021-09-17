let inertia =
  Dream_inertia.init
    ~base_url:Uri.(of_string "http://localhost:8080")
    ~version:None ~template:Index.render ()

let () =
  let content =
    Yojson.Safe.from_string
      {json|
  {"event":{"id":80,"title":"Birthday party","start_date":"2019-06-02","description":"Come out and celebrate Jonathan&apos;s 36th birthday party!"}}
  |json}
  in
  let data = Dream_inertia.{component= "Hello"; props= content} in
  let handler = Dream_inertia.inertia_handler inertia (fun _req -> data) in
  Dream.run ~adjust_terminal:false
  @@ Dream.router [Dream.get "/test/hello" handler]
  @@ Dream.not_found
