type frag = frag Xmlm.frag

let parse_namespaced s =
  try
    let n = String.index s '\'' in
    (String.sub s 0 n, String.sub s (n + 1) (String.length s - n - 1))
  with
  | Not_found -> ("", s)

let string_of_char = String.make 1

let string_of_atom x =
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
  | `List x -> invalid_arg "Expected atom, received list."

let rec parse_attrs attrs =
  match attrs with
  | `List [`List [ns; k]; v] :: rest ->
    ((string_of_atom ns, string_of_atom k), string_of_atom v) :: parse_attrs rest
  | `List [k; v] :: rest ->
    (("", string_of_atom k), string_of_atom v) :: parse_attrs rest
  | [] -> []
  | _ -> invalid_arg "Provided SXML attributes are in an invalid format."

let rec xmlm_of_sexp ?(sigil="~+") x =
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
