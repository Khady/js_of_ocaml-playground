module Html = Dom_html

let (>>=) = Lwt.bind

let debug_mode = true

let debug fmt =
  Printf.ksprintf (fun s ->
      if debug_mode
      then Firebug.console##log(Js.string s)
      else ()
    ) fmt

let update id value =
  let d = Html.document in
  let node =
    Js.Opt.get (d##getElementById(Js.string id))
      (fun () -> assert false) in
  Js.Unsafe.set node "value" value

let handler from_id to_id element _ =
  let d = Html.document in
  let from =
    Js.Opt.get (d##getElementById(Js.string from_id))
      (fun () -> assert false) in
  let temp: Js.js_string Js.t = Js.Unsafe.get from "value" in
  let () = if temp = Js.string "" then
      debug "oups"
    else
      let t = Js.parseFloat temp in
      Firebug.console##log(t);
      debug "ok"
  in
  let _ = update to_id temp in
  Lwt.return ()

let onload _ =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById(Js.string "wiki_demo"))
      (fun () -> assert false) in
  let celsius = Html.createInput
      ?_type:(Some (Js.string "number"))
      d in
  celsius##placeholder <- Js.string "number";
  celsius##id <- Js.string "celsius";
  celsius##defaultValue <- Js.string "0";
  let fahrenheit = Html.createInput
      ?_type:(Some (Js.string "number"))
      d in
  fahrenheit##placeholder <- Js.string "number";
  fahrenheit##id <- Js.string "fahrenheit";
  fahrenheit##defaultValue <- Js.string "32";
  Dom.appendChild body celsius;
  Dom.appendChild body fahrenheit;
  Lwt.ignore_result (Lwt_js_events.inputs celsius (handler "celsius" "fahrenheit"));
  Lwt.ignore_result (Lwt_js_events.inputs fahrenheit (handler "fahrenheit" "celsius"));
  Js._false

let _ =
  Html.window##onload <- Html.handler onload
