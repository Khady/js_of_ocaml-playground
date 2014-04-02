module Html = Dom_html

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

let convert_F_to_C f =
  (f -. 32.0) /. 1.8

let convert_C_to_F c =
  (c *. 1.8) +. 32.0

let switch _ _ =
  let d = Html.document in
  let celsius =
    Js.Opt.get (d##getElementById(Js.string "celsius"))
      (fun () -> assert false) in
  let fahrenheit =
    Js.Opt.get (d##getElementById(Js.string "fahrenheit"))
      (fun () -> assert false) in
  let celsius_value: Js.js_string Js.t = Js.Unsafe.get celsius "value" in
  let fahrenheit_value: Js.js_string Js.t = Js.Unsafe.get fahrenheit "value" in
  Js.Unsafe.set celsius "value" fahrenheit_value;
  Js.Unsafe.set fahrenheit "value"celsius_value;
  Lwt.return ()

let handler from_id to_id convert _ _ =
  let d = Html.document in
  let from =
    Js.Opt.get (d##getElementById(Js.string from_id))
      (fun () -> assert false) in
  let temp: Js.js_string Js.t = Js.Unsafe.get from "value" in
  let () = if temp = Js.string "" then
      debug "oups"
    else
      let t = Js.parseFloat temp in
      let _ = update to_id (convert t) in
      Firebug.console##log(t);
      debug "ok"
  in
  Lwt.return ()

let load _ =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById(Js.string "temperature"))
      (fun () -> assert false) in
  let button = Html.createInput
      ?_type:(Some (Js.string "button"))
      ?name:(Some (Js.string "switch"))
      d in
  let () = button##value <- Js.string "switch values" in
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
  Dom.appendChild body (Html.createBr d);
  Dom.appendChild body button;
  Dom.appendChild body (Html.createBr d);
  Dom.appendChild body fahrenheit;
  Lwt.ignore_result (Lwt_js_events.inputs
                       celsius
                       (handler
                          "celsius"
                          "fahrenheit"
                          convert_C_to_F
                       )
                    );
  Lwt.ignore_result (Lwt_js_events.inputs
                       fahrenheit
                       (handler
                          "fahrenheit"
                          "celsius"
                          convert_F_to_C
                       )
                    );
  Lwt.ignore_result (Lwt_js_events.clicks button switch);
  celsius##focus ();
  Js._false

let _ =
  Html.window##onload <- Html.handler load
