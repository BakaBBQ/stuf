open Printf;;
open Omd;;
open Str;;

let output = "index.html"

let default_css = "



body {
  font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  margin-left: 5em;
  margin-right: 5em;
  margin-top: 3em;
  line-height: 24px;
}

h1 {
  color: #2C3E50;
  font-size: 3em;
}

h2 {
  margin: 12px 0;
  color: #34495E;
  padding-top: 1em;
  border-top: 1px solid #ccc;
}

h3, h4, h5, h6{
  color: #34495E;
  padding-top: 0.3em;
}

a {
  font-size: 80%;
  color: #7F8C8D;
}

/*http://adis.ca/article/pretty-code-block-in-css*/
pre {
    font-family: 'Courier 10 Pitch', Courier, monospace;
    font-size: 95%;
    line-height: 140%;
    white-space: pre;
    white-space: pre-wrap;
    white-space: -moz-pre-wrap;
    white-space: -o-pre-wrap;
}

code {
    font-family: 'Courier 10 Pitch', Courier, monospace;
    font-size: 95%;
    line-height: 140%;
    white-space: pre;
    white-space: pre-wrap;
    white-space: -moz-pre-wrap;
    white-space: -o-pre-wrap;
    background: #34495E;
    display: block;
    padding: 0.5em 1em;
    border: 1px solid #bebab0;
    color: #ECF0F1;
}


"

(* http://stackoverflow.com/questions/11193783/ocaml-strings-and-substrings*)
let is_substring string substring =
  let ssl = String.length substring and sl = String.length string in
  if ssl = 0 || ssl > sl then false else
    let max = sl - ssl and clone = String.create ssl in
    let rec check pos =
      pos <= max && (
        String.blit string pos clone 0 ssl ; clone = substring
        || check (String.index_from string (succ pos) substring.[0])
      )
    in
    try check (String.index string substring.[0])
    with Not_found -> false

type stuf_file =
  {
    name : string;
    desc : string;
    path : string;
  };;

let output = "index.html"

let path = Sys.getcwd()
let directory = (Filename.concat path "files")
let css_path = Filename.concat path "main.css"

let slurp fullpath =
  let file = open_in fullpath in
  let size = in_channel_length file in
  let buf = Bytes.create size in
  begin
    really_input file buf 0 size;
    buf
  end

let load_file name desc path = {name=name; desc=desc; path=path}

let load_fileU fullpath =
  let name = (Filename.basename fullpath) in
  let path = (Filename.concat "files/" name) in
  let desc_path = (fullpath ^ ".desc") in
  let desc =
    if Sys.file_exists desc_path then
      (slurp desc_path)
    else
      "No description avaliable." in
  (load_file name desc path)

let readdirL = (List.map (fun x -> (Filename.concat directory x)) (Array.to_list (Sys.readdir directory)))

(*this is a battery included function, call this and you will be fine*)
let load_files_to_memory =
  let f x = (not (Sys.is_directory x)) in
  let g x = (not (is_substring x ".desc")) in
  let h x = (not ((String.get (Filename.basename x) 0) = '.')) in
  let fn x = (g x) && (f x) && (h x) in
  let filtered_files = (List.filter fn (readdirL)) in
  (List.map load_fileU filtered_files);;

let markdown_download_link url = "[Download]" ^ "(" ^ url ^ ")"

let readme = slurp (Filename.concat path "README.md")

let generate_markdown filedata =
  "##" ^ filedata.name ^ "\n\n" ^ (markdown_download_link filedata.path) ^ "\n\n" ^ filedata.desc ^ "\n"

let generate_markdowns = List.map generate_markdown

let assemble_files_to_markdown files =
  let individual_mds = (List.map generate_markdown files) in
  (readme ^ (String.concat "\n" individual_mds))

let get_markdown = load_files_to_memory |> assemble_files_to_markdown

let md_to_html x = (Omd.of_string x) |> Omd.to_html
let get_html = get_markdown |> md_to_html

let gen_html title contents =
"
<!DOCTYPE html>
<html lang=\"en\">
<head>
	<meta charset=\"utf-8\">
	<title>" ^ title ^ "</title>
  <link href=\"main.css\" rel=\"stylesheet\" media=\"all\">
</head>
<body>
	" ^ contents ^ "
</body>
</html>
"



let title = (List.hd (Str.split (Str.regexp "\n+") readme))

let patholize x = (Filename.concat directory x)
(* let get_files_from_dir x = (readdirL x) |> (List.filter (fun x -> (not (Sys.is_directory (patholize x)))))*)
let () =
  let css_exists = Sys.file_exists css_path in
  let oc = open_out output in
  let oc2 = open_out "index.md" in
  fprintf oc "%s\n" (gen_html title (get_html));
  fprintf oc2 "%s\n" get_markdown;
  if (not css_exists) then
    let occss = open_out css_path in
    fprintf occss "%s\n" default_css;
    close_out occss;
  close_out oc;
  close_out oc2;
