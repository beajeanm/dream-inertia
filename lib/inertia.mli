module Page : sig
  type deferred_prop

  type t = {
    component : string;
    props : (string * Yojson.Safe.t) list;
    deferred_props : deferred_prop list;
    url : string;
    merge_props : string list;
    prepend_props : string list;
    match_on : string list;
  }
end

val with_prop : Page.t -> string * Yojson.Safe.t -> Page.t
(** [with_prop page prop] create a copy of the page with the property added to
    it. *)

val with_defer_prop : Page.t -> Page.deferred_prop -> Page.t
(** [with_defer_prop page defer] create a copy of the page with the deferred
    property added to it. See {!Inertia.defer} *)

val defer :
  ?group:string -> key:string -> (unit -> Yojson.Safe.t) -> Page.deferred_prop
(** [defer ?group key callback] create a deferred property that can be added to
    a page.
    @param group
      Grouping deferred properties allows them to be loaded in parallel. Each
      group will be loaded by a separate request. Default ["default"]
    @see <https://inertiajs.com/deferred-props>*)

val page :
  component:string ->
  ?props:(string * Yojson.Safe.t) list ->
  ?deferred_props:Page.deferred_prop list ->
  ?merge_props:string list ->
  ?prepend_props:string list ->
  ?match_on:string list ->
  url:string ->
  unit ->
  Page.t
(** [page component props URL ()] create a new page.
    @see <https://inertiajs.com/pages> *)

val render :
  ?headers:(string * string) list ->
  ?status:Dream.status ->
  Dream.request ->
  Page.t ->
  Dream.response Lwt.t
(** [render ~headers ~status request page] Create a new Dream.response by
    rendering the page data. The page will be rendered either as text/html for
    standard visit, or application/json for inertia request *)

val middleware :
  version:string -> root_view:(string -> string) -> unit -> Dream.middleware
(** [middleware version root_view ()] initialize inertiajs and return a
    middleware that should be added to Dream's middleware stack.
    @param version The version to use for assets version
    @see <https://inertiajs.com/asset-versioning>
    @param root_view
      A function used to generate the HTML response on first request. It will
      receive a string representation of the page data to render. See
      {!Helper.root_view}
    @see <https://inertiajs.com/server-side-setup#root-template>*)

val add_error : Dream.request -> string -> Yojson.Safe.t -> unit
(** [add_error request key error] add flash message error. The data added
    through that method will be use to populate a property named errors that can
    be use in the view templates. It requires the [Dream.flash] middleware to be
    present.
    @param key
      The key for that error, if an error with that key is already presents it
      will be replaced.*)

val add_shared_data : Dream.request -> string -> Yojson.Safe.t -> unit Lwt.t
(** [add_shared_data request key value] add a shared data property to the
    current user session.
    @see <https://inertiajs.com/shared-data> *)

val get_shared_data : Dream.request -> string -> Yojson.Safe.t option
(** [get_shared_data request key] return the shared data associated with that
    key if it exists. *)

val all_shared_data : Dream.request -> (string * Yojson.Safe.t) list
(** [all_shared_data request] return all the shared data associated with the
    current session. *)

val flush_shared_data : Dream.request -> unit Lwt.t
(** [flush_shared_data request] flush all the shared data. *)

(** Helper module, it contains default implementation you may want to
    re-implement for your use cases. *)
module Helper : sig
  val root_view : js:string -> css:string -> string -> string
  (** [create js css] create a root view for standard visit. The template is
      basic but should be sufficient for most usage.

      [js] the path to the JavaScript asssets.

      [css] the path to the CSS assets*)
end
