type frag = frag Xmlm.frag

let string_of_char = String.make 1

let rec string_of_atom x =
  match x with
  | `Char x -> string_of_char x
  | `Float x -> x
  | `Symbol x -> x
  | `Int x -> string_of_int x
  | `Int32 x -> Int32.to_string x
  | `Int64 x -> Int64.to_string x
  | `Nativeint x -> Nativeint.to_string x
  | `String x -> x
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `List _ -> invalid_arg ("Expected atom, received list: " ^ (string_of_sexp x))
and string_of_sexp x =
  match x with
  | `String x -> "\"" ^ x ^ "\""
  | `List xs -> "(" ^ (String.concat " " (List.map string_of_sexp xs)) ^ ")"
  | _ -> string_of_atom x

let rec parse_attrs attrs =
  match attrs with
  | `List [`List [ns; k]; v] :: rest ->
    ((string_of_atom ns, string_of_atom k), string_of_atom v) :: parse_attrs rest
  | `List [k; v] :: rest ->
    (("", string_of_atom k), string_of_atom v) :: parse_attrs rest
  | [] -> []
  | _ -> invalid_arg ("Provided SXML attributes are in an invalid format: " ^ (string_of_sexp (`List attrs)))

let rec xmlm_of_sexp ?(sigil="@") x =
  let xmlm_of_el ?(ns'=`String "") tag' children' =
    let attrs, children = match children' with
      | `List (`Symbol sigil' :: attrs) :: children when sigil' = sigil ->
        parse_attrs attrs, children
      | children ->
        [], children
    in
    let ns = string_of_atom ns' in
    let tag = string_of_atom tag' in
    `El (((ns, tag), attrs), List.map (xmlm_of_sexp ~sigil) children)
  in
  match x with
  | `List (`List [ns'; tag'] :: children') ->
    xmlm_of_el ~ns' tag' children'
  | `List (tag' :: children') ->
    xmlm_of_el tag' children'
  | _ -> `Data (string_of_atom x)
