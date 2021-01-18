open! Import

module Index = struct
  type t =
    | Root
    | Child of {
        parent_path : Safe_string.t list;
        name : Safe_string.t;
        last : bool;
      }

  let root = Root

  let rcons t ~last ~name =
    let parent_path =
      match t with
      | Root -> []
      | Child { parent_path; name; _ } -> parent_path @ [ name ]
    in
    Child { parent_path; name; last }

  let last_in_parent = function Root -> false | Child { last; _ } -> last

  let parent_path = function
    | Root -> failwith "Root node has no parent"
    | Child { parent_path; _ } -> parent_path

  let leaf_name = function
    | Root -> failwith "Root node has no label"
    | Child { name; _ } -> name
end

module Run_result = struct
  type t =
    | Ok
    | Exn of Index.t * string * unit Fmt.t
    | Error of Index.t * unit Fmt.t
    | Skip
    | Todo of string

  let is_failure = function
    | Ok | Skip -> false
    | Error _ | Exn _ | Todo _ -> true

  let has_run = function Ok | Error _ | Exn _ -> true | Skip | Todo _ -> false
end

module Suite (M : Monad.S) = struct
  module String_set = Set.Make (String)
  module M = Monad.Extend (M)
  open M.Syntax

  type filter_result = [ `Run | `Skip ]
  type filter = Tag.Set.t -> filter_result

  type 'a test = {
    label : Safe_string.t;
    loc : Source_code_position.pos option;
    tags : Tag.Set.t;
    node : 'a test_node;
  }

  and 'a test_node = Test of ('a -> unit M.t) | Group of 'a test list

  let test ~name ~loc ~tags fn =
    let label = Safe_string.v name in
    { label; loc; tags; node = Test fn }

  let group ~name ~loc ~tags children =
    let label = Safe_string.v name in
    { label; loc; tags; node = Group children }

  type 'a t = {
    identifier : Safe_string.t;
    name : Safe_string.t option;
    file : string option;
    loc : Source_code_position.pos option;
    tests : 'a test list;
  }

  let validate_tests =
    let rec aux ctx ts =
      let open Result.Syntax in
      let* () =
        let duplicate =
          ts
          |> List.map (fun { label; _ } -> label)
          |> List.find_duplicate ~compare:Safe_string.compare
        in
        match duplicate with
        | None -> Ok ()
        | Some dup ->
            let path =
              List.rev_map Safe_string.to_string (dup :: ctx)
              |> String.concat ~sep:" › "
            in
            Error (`Duplicate_path path)
      in
      List.fold_result ts ~init:() ~f:(fun () -> function
        | { node = Test _; _ } -> Ok ()
        | { node = Group children; label; _ } -> aux (label :: ctx) children)
    in
    fun ts -> aux [] ts

  let of_tests ~name ~file ~loc tests =
    let open Result.Syntax in
    let* () = validate_tests tests in
    match (name, file) with
    | Some x, _ when x = "" -> Error `Empty_name
    | _, Some x when x = "" -> Error `Empty_file
    | None, None -> Error `No_identifier
    | Some identifier, _ | _, Some identifier ->
        let identifier = Safe_string.v identifier in
        let name = Option.map Safe_string.v name in
        Ok { identifier; name; file; loc; tests }

  let rec list_fold_until ~f ~init:acc ~finish = function
    | [] -> M.return (finish acc)
    | [ x ] -> (
        f ~last:true acc x >|= function Stop c -> c | Continue c -> finish c)
    | x :: (_ :: _ as xs) -> (
        f ~last:false acc x >>= function
        | Stop c -> M.return c
        | Continue acc -> list_fold_until ~f ~init:acc ~finish xs)

  let foldi_until ~filter ?group ?test ~init:acc ~finish t =
    let fold_default _ acc _ = M.return (Continue acc) in
    let group = Option.value group ~default:fold_default
    and test = Option.value test ~default:fold_default in
    let rec aux ~last ~index acc { label; tags; node; _ } =
      let index = Index.rcons index ~last ~name:label in
      let filter_result = filter tags in
      match node with
      | Test fn ->
          let arg =
            match filter_result with `Run -> `Run fn | `Skip -> `Skip
          in
          test index acc arg
      | Group children -> (
          M.bind (group index acc filter_result) @@ function
          | Stop _ as x -> M.return x
          | Continue acc -> (
              match filter_result with
              | `Skip -> M.return (Continue acc)
              | `Run ->
                  list_fold_until children ~init:acc
                    ~f:(fun ~last acc child ->
                      aux ~last ~index acc child >|= function
                      | Continue _ as c -> c
                      | Stop _ as s -> Stop s)
                    ~finish:(fun x -> Continue x)))
    in
    list_fold_until t.tests ~init:acc ~finish ~f:(aux ~index:Index.root)

  let fold ~filter ~group ~test ~init t =
    foldi_until t ~filter ~init
      ~finish:(fun x -> x)
      ~group:(fun _ a x -> M.return (Continue (group a x)))
      ~test:(fun _ a x -> M.return (Continue (test a x)))

  let identifier { identifier; _ } = Safe_string.to_string identifier
  let pp_name { name; _ } = Option.map (Fun.flip Safe_string.pp) name
  let pp_file { file; _ } = Option.map (Fun.flip Fmt.string) file

  let pp_tags ppf ts =
    let open Fmt in
    string ppf "< ";
    list ~sep:(const string "; ") Tag.pp ppf (Tag.Set.to_list ts);
    Fmt.string ppf " >"

  let rec pp_nodes ~pre ~last_dir ppf files =
    let open Fmt in
    (* Only print newline at the end if our ancestors have not already done so
       (i.e. we are not the descendant of a last directory *)
    let pp_last_dir ppf last_dir = if not last_dir then pf ppf "@,%s" pre in
    let pp_children_last ppf =
      pf ppf "%s└─ %a" pre (pp_node ~last_dir:true ~pre:(pre ^ "   "))
    and pp_children_not_last ppf =
      pf ppf "%s├─ %a" pre (pp_node ~last_dir:false ~pre:(pre ^ "│  "))
    in
    match files with
    | [] -> ()
    | [ last ] -> pf ppf "@,%a%a" pp_children_last last pp_last_dir last_dir
    | _ :: _ :: _ ->
        let last, not_last =
          match List.rev files with
          | [] -> assert false
          | last :: not_last_rev -> (last, List.rev not_last_rev)
        in
        pf ppf "@,%a@,%a%a"
          (list ~sep:cut pp_children_not_last)
          not_last pp_children_last last pp_last_dir last_dir

  and pp_node ~pre ~last_dir ppf =
    let open Fmt in
    let pp_group_name = styled `Bold (styled `Blue Safe_string.pp) in
    function
    | { node = Test _; label; tags; _ } ->
        Fmt.pf ppf "%a  %a" Safe_string.pp label
          Fmt.(styled `Faint pp_tags)
          tags
    | { node = Group children; label; _ } ->
        pp_group_name ppf label;
        pp_nodes ~pre ~last_dir ppf children

  let pp ppf t =
    pp_node ~pre:"" ~last_dir:false ppf
      {
        node = Group t.tests;
        label = t.identifier;
        loc = None;
        tags = Tag.Set.empty;
      }
end
