(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module Node =
struct
  type ('a,'b) t =  {
    key: 'a;
    value: 'b option;
    children: ('a,'b) t list;
  }

  let create key value = {
    key = key;
    value = Some value;
    children = [];
  }

  let empty key = {
    key = key;
    value = None;
    children = []
  }

  let get_key node = node.key
  let get_value node =
    match node.value with
    | None       -> raise Not_found
    | Some value -> value

  let get_children node = node.children

  let set_value node value =
    { node with value = Some value }
  let set_children node children =
    { node with children = children }

  let add_child node child =
    { node with children = child :: node.children }
end

type ('a,'b) t = {
  children: ('a,'b) Node.t list;
  value: 'b option;
}

let mem_node nodes key =
  List.exists (fun n -> n.Node.key = key) nodes

let find_node nodes key =
  List.find (fun n -> n.Node.key = key) nodes

let replace_node nodes key node =
  let rec aux = function
    | []                            -> []
    | h :: tl when h.Node.key = key -> node :: tl
    | h :: tl                       -> h :: aux tl
  in
  aux nodes

let remove_node nodes key =
  let rec aux = function
    | []                            -> raise Not_found
    | h :: tl when h.Node.key = key -> tl
    | h :: tl                       -> h :: aux tl
  in
  aux nodes

let create () = { children = []; value = None; }

let rec iter f tree =
  let rec aux node =
    f node.Node.key node.Node.value;
    List.iter aux node.Node.children
  in
  List.iter aux tree.children

let rec list_map_filter f = function
  | [] -> []
  | h :: tl -> match f h with
      | Some h -> h :: list_map_filter f tl
      | None -> list_map_filter f tl

let map f tree =
  let rec aux value children = {
    value = (
      match value with
      | None       -> None
      | Some value -> f value
    );
    children = (
      list_map_filter
        (fun n -> match aux n.Node.value n.Node.children with
          | { value = None; children = [] } -> None
          | { value; children } ->
              Some {Node. key = n.Node.key; value; children})
        children
    )
  }
  in
  aux tree.value tree.children

let rec fold f tree acc =
  let rec aux accu node =
    List.fold_left aux (f node.Node.key node.Node.value accu) node.Node.children
  in
  List.fold_left aux acc tree.children

let rec fold_paths f tree acc =
  let rec aux acc children value path =
    let acc =
      List.fold_left
        (fun acc n -> aux acc n.Node.children n.Node.value (n.Node.key::path))
        acc
        children
    in
    match value with Some v -> f acc path v | None -> acc
  in
  aux acc tree.children tree.value []

(* return a sub-trie *)
let rec sub_node tree path =
  let rec aux children value = function
    | []      -> { children; value }
    | h :: tl ->
        let n = find_node children h in
        aux n.Node.children n.Node.value tl
  in
  aux tree.children tree.value path

let sub tree path =
  try sub_node tree path
  with Not_found -> { children = []; value = None }

let find tree path =
  match (sub_node tree path).value with
  | Some v -> v
  | None -> raise Not_found

(* return false if the node doesn't exists or if it is not associated to any value *)
let mem tree path =
  let rec aux children = function
    | []   -> false
    | h::t ->
        mem_node children h
        && (let node = find_node children h in
          if t = []
          then node.Node.value <> None
          else aux node.Node.children t)
  in
  aux tree.children path

(* Iterate over the longest valid prefix *)
let iter_path f tree path =
  let rec aux children = function
    | []   -> ()
    | h::l ->
        if mem_node children h
        then begin
          let node = find_node children h in
          f node.Node.key node.Node.value;
          aux node.Node.children l
        end
  in
  aux tree.children path

let set tree path value =
  let rec aux children node_value = function
    | []   -> { children; value = Some value }
    | h::t ->
        let children, found =
          List.fold_right
            (fun n (acc,found) ->
              if n.Node.key = h then
                let { children; value } =
                  aux n.Node.children n.Node.value t
                in
                { n with Node. children; value } :: acc, true
              else n :: acc, found)
            children
            ([], false)
        in
        if found then { children; value = node_value }
        else
          let sub = aux [] None t in
          { children =
              { Node. key = h; value = sub.value; children = sub.children }
              :: children;
            value = node_value }
  in
  aux tree.children tree.value path

let unset tree =
  let rec aux children value = function
    | []   -> { children; value = None }
    | h::t ->
        { children = (
            list_map_filter
              (fun n ->
                if n.Node.key = h then
                  match aux n.Node.children n.Node.value t with
                  | { value = None; children = [] } -> None
                  | { value; children } ->
                      Some { n with Node. value; children }
                else Some n)
              children
          );
          value }
  in
  aux tree.children tree.value

let filter f tree =
  let rec aux children value =
    { children =
        list_map_filter
          (fun n ->
            if f n.Node.key then
              let { children; value } = aux n.Node.children n.Node.value in
              Some { n with Node. children; value }
            else None)
          children;
      value }
  in
  aux tree.children tree.value
