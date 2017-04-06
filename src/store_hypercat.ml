open Lwt

module R = Rresult.R

type relation = {
  rel   : string;
  value : string
}

type item = {
  item_metadata : relation list;
  href          : Uri.t }


let relation =
  let open Depyt in
    record "relation" (fun rel value -> {rel; value})
    |+ field "rel" string (fun r -> r.rel)
    |+ field "val" string (fun r -> r.value)
    |> sealr

let item =
  let open Depyt in
  let uri = like string Uri.of_string Uri.to_string in
  record "item" (fun item_metadata href -> {item_metadata; href})
  |+ field "item-metadata" (list relation) (fun i -> i.item_metadata)
  |+ field "href" uri (fun i -> i.href)
  |> sealr


let decode_item body =
  let decoder = Jsonm.decoder (`String body) in
  Depyt.decode_json item decoder


let check_hypercat rels =
  let has_desp r =
    let affix = "urn:X-hypercat:rels:hasDescription:" in
    let len = String.length affix in
    String.length r.rel > len && String.sub r.rel 0 len = affix
  in
  let has_cy r = r.rel = "urn.X-hypercat:rels:isContentType" in
  List.exists has_desp rels
  && List.exists has_cy rels


let check_databox rels =
  let required = [
    "urn:X-databox:rels:hasVendor";
    "urn:X-databox:rels:hasType";
    "urn:X-databox:rels:hasDatasourceid";
    "urn:X-databox:rels:hasStoreType";
  ] in
  List.for_all (fun req ->
    List.exists (fun r -> r.rel = req) rels) required


let validate_item body =
  R.bind (decode_item body) (fun i ->
      if not @@ check_hypercat i.item_metadata then R.error "check_hypercat"
      else if not @@ check_databox i.item_metadata then R.error "check_databox"
      else R.ok i)



type catalogue = {
  catalogue_metadata : relation list;
  mutable items      : item list
}


let catalog =
  let open Depyt in
  record "catalogue" (fun c i -> {catalogue_metadata = c; items = i})
  |+ field "catalogue-metadata" (list relation) (fun c -> c.catalogue_metadata)
  |+ field "items" (list item) (fun c -> c.items)
  |> sealr


let encode_catalogue c =
  let buf = Buffer.create 512 in
  let dst = `Buffer buf in
  let encoder = Jsonm.encoder dst in
  Depyt.encode_json catalog encoder c;
  Buffer.contents buf


let cat =
  let catalogue_metadata = [{
      rel   = "urn:X-hypercat:rels:isContentType";
      value = "application/vnd.hypercat.catalogue+json";
    };{
      rel   = "urn:X-hypercat:rels:hasDescription:en";
      value = "Databox Datasource Catalogue";
    };{
      rel   = "urn:X-hypercat:rels:hasHomepage";
      value = "http://www.databoxproject.uk/";
    };{
      rel   = "urn:X-hypercat:rels:containsContentType";
      value = "application/vnd.hypercat.catalogue+json"
    };{
      rel   = "urn:X-hypercat:rels:supportsSearch";
      value = "urn:X-hypercat:search:simple"
    }] in
  {catalogue_metadata; items = []}


let handler meth ~body =
  let module C = Cohttp in
  match meth with
  | `GET ->
      let headers = C.Header.init_with "Content-Type" "application/json" in
      let resp = C.Response.make ~headers () in
      let body =
        encode_catalogue cat
        |> Cohttp_lwt_body.of_string
      in
      return (resp, body)
  | `POST ->
      let validity = validate_item body in
      if R.is_ok validity then
        let item = R.get_ok validity in
        let new_item = ref true in
        let items' = List.fold_left (fun acc i ->
            if 0 <> Uri.compare i.href item.href then i :: acc
            else begin new_item := false; item :: acc end) [] cat.items
        in
        let () =
          if !new_item then cat.items <- item :: cat.items
          else cat.items <- items'
        in
        let status =
          (if !new_item then 201 else 200)
          |> C.Code.status_of_code
        in
        let headers =
          let location = Printf.sprintf "https://%s:%s"
              (Store_env.local_name ()) (Store_env.local_port ()) in
          C.Header.init_with "location" location
        in
        let resp = C.Response.make ~status ~headers () in
        return (resp, Cohttp_lwt_body.empty)
      else let msg = R.get_error validity in
        let status = C.Code.status_of_code 400 in
        let resp = C.Response.make ~status () in
        let body = Cohttp_lwt_body.of_string msg in
        return (resp, body)
  | _ ->
      let resp = C.Response.make ~status:`Not_found () in
      let body =
        let msg =  Printf.sprintf "unsupported method %s on hypercat"
            (C.Code.string_of_method meth) in
        Cohttp_lwt_body.of_string msg
      in
      return (resp, body)
