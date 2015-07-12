(*  chkenv - check environment

    Copyright (c) 2014-2015 Marc Butler <mockbutler@gmail.com>
    All Rights Reserved.

    To build:
    ocamlc -o chkenv unix.cma str.cma chkenv.ml
 *)

open Filename;;
open Printf;;
open Unix;;

(*  Canonize a file system path.
 *)
let canonical_path path =
  if Filename.is_relative path then
    Filename.concat Filename.current_dir_name path
  else path;;

(*  Follow one or more links to return the stat info of the final target
    file entry.
    Will raise an error if circular symbolic references are detected.
 *)
let follow_links path =
  let rec follow sofar path =
    let full_path = canonical_path path in
    let stinfo = Unix.lstat full_path in
      if stinfo.st_kind = Unix.S_LNK then
        if List.exists (fun s -> s = full_path) sofar then
          failwith ("circular symlink: " ^ (List.hd (List.rev sofar)))
        else
          follow (full_path :: sofar) (readlink path)
      else
        stinfo in
  follow [] path;;

(*  Predicate returns true if the path is a directory.
    Follows chains of symbolic links.
 *)
let is_dir path =
  let stinfo = follow_links path in
    stinfo.st_kind = S_DIR;;

(* Check and report that the path is a directory. *)
let dircheck path =
  try
    let stinfo = follow_links path in
    match stinfo.st_kind with
      S_REG -> printf "%s is a file not a directory\n" path
    | S_DIR -> ()
    | _ -> printf "%s irregular file type\n" path
  with Unix_error(err, fn, arg) ->
    printf "%s %s is missing\n" fn path
  | Failure(msg) -> printf "%s\n" msg;;

(* Return the PATH environment as a list of paths. *)
let get_path =
  Str.split (Str.regexp ":") (Unix.getenv "PATH");;

(* Entry point. *)
let _ =
  let dirs = get_path in
  List.iter dircheck dirs ;;
