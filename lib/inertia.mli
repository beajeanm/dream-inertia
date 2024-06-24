type t
(** The type of the inertiajs driver. *)

type page
(** The type of a page, see {{:https://inertiajs.com/the-protocol#the-page-object} The page object}. *)

type prop
(** The type of the properties used to populate the page.

    There are four types of properties the differ in how they are evaluated and interact with partial reload,
    see {{:https://inertiajs.com/partial-reloads#lazy-data-evaluation} Lazy data evaluation}.
    - Regular properties: {b Always} included on standard visits, {b optionally} included on partial reloads
    - Delayed properties: {b Always} included on standard visits, {b optionally} included on partial reloads,
    evaluated when {b needed}
    - Lazy properties: NEVER included on standard visits, {b optionally} included on partial reloads,
    evaluated when {b needed}
    - Always properties: {b Always} included on standard visits, {b always} included on partial reloads

    *)

val prop : Yojson.Safe.t -> prop
(** Create a regular property. *)

val lazy_prop : (unit -> Yojson.Safe.t) -> prop
(** Create a lazy property. *)

val delayed_prop : (unit -> Yojson.Safe.t) -> prop
(** Create a delayed property. *)

val always_prop : Yojson.Safe.t -> prop
(** Create an always property. *)

val init : version:string -> js_path:string -> css_path:string -> unit -> t
(** [init version js_path css_path ()] initialize a driver with the asset version set to [version],
    [js_path] and [css_path] will be use to generate the rootview. *)

val middleware : t -> Dream.middleware
(** Manage assets versioning, csfr token, and non post redirection status. *)

val page :
  component:string -> ?props:(string * prop) list -> url:string -> unit -> page
(** [page component props url ()] create a new page data. *)

val with_prop : page -> string -> prop -> page
(** [with_prop page key prop] create a copy of the page with this property added,
    if the key already exists it will be duplicated. *)

val with_url : page -> string -> page
(** [with_url page url ] create a copy of the page with a new url. *)

val inertia_response :
  t ->
  ?headers:(string * string) list ->
  ?status:Dream.status ->
  Dream.request ->
  page ->
  Dream.response Lwt.t
(** Create a new Dream response by rendering the page data.
    The page will be rendered either as text/html for standard visit, or application/json for inertia request *)
