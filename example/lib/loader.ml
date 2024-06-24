let index_css =
  Assets.Internal.file_list
  |> List.filter (fun name ->
         String.starts_with ~prefix:"assets/index" name
         && String.ends_with ~suffix:"css" name)
  |> List.hd

let index_js =
  Assets.Internal.file_list
  |> List.filter (fun name ->
         String.starts_with ~prefix:"assets/index" name
         && String.ends_with ~suffix:"js" name)
  |> List.hd

let version =
  (* Skip assets/index- at the beginning and .js at the end*)
  String.sub index_js 13 (String.length index_js - 16)

let asset_loader directory path req =
  let resource = Format.asprintf "%s%s" directory path in
  match (Dream.header req "If-None-Match", Assets.hash resource) with
  | Some request_etag, Some etag when String.equal request_etag etag ->
      Dream.empty `Not_Modified
  | _, Some hash ->
      (* Assets.read is guarantee to be Some since Assets.hash is*)
      let file = Assets.read resource |> Option.get in
      let content_type_header = Dream.mime_lookup path in
      (* Caching for a week *)
      let max_age = 604800 in
      let cache_headers =
        [
          ("ETag", hash);
          ("Cache-Control", Stdlib.Format.asprintf "max-age=%i" max_age);
        ]
      in
      Dream.respond
        ~headers:(List.append content_type_header cache_headers)
        file
  | _, None -> Dream.empty `Not_Found
