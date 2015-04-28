
let r_open_in s = 
  let ic = open_in s in
  let size = in_channel_length ic in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let xpm_label_box ~file ~text ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and label and pack *)
  let box = GPack.hbox ~border_width:2 ~packing () in

  (* Now on to the image stuff and pack into box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  ignore(GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ());
  
  (* Create a label for the button and pack into box *)
  GMisc.label ~text ~packing:(box#pack ~padding:3) ()
    
let scrolled_man = ref (GBin.scrolled_window
			~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
			())
let source_man = ref
  (GSourceView2.source_view
     ~width:500 ~height:300
     ~packing:!scrolled_man#add 
     ())
let l = GSourceView2.source_language_manager ~default:true

let lang = l#guess_language ~content_type:"text/casl" () 
let _ = ignore(!source_man#source_buffer#set_language lang)


let vpain_man = ref (GPack.paned `HORIZONTAL ())

let options () =
   let windowoptions = GWindow.window ~title:"External Options" 
    ~width:515 ~height:250 ~modal:true ~border_width:10 () in
  let vbox = GPack.vbox ~packing:windowoptions#add () in
  let hbox1 = GPack.hbox ~packing:vbox#pack () in
  let hbox2 = GPack.hbox ~packing:vbox#pack () in
  let hbox3 = GPack.hbox ~packing:vbox#pack () in
  let hbox4 = GPack.hbox ~packing:vbox#pack () in
  let hbox5 = GPack.hbox ~packing:vbox#pack () in
  let hbox6 = GPack.hbox ~packing:vbox#pack () in
  let hbox7 = GPack.hbox ~packing:vbox#pack () in
  let hbox8 = GPack.hbox ~packing:vbox#pack () in
  ignore(GMisc.label ~text:"numModels" ~packing:hbox1#add ());
  ignore(GEdit.entry ~text:"10" ~max_length:2 ~packing:hbox1#add ());
  ignore(GMisc.label ~text:"minIterationsGeneralize" ~packing:hbox2#add ());
  ignore(GEdit.entry ~text:"1" ~max_length:2 ~packing:hbox2#add ());
  ignore(GMisc.label ~text:"maxIterationsGeneralize = 20" ~packing:hbox3#add ());
  ignore(GEdit.entry ~text:"20" ~max_length:2 ~packing:hbox3#add ());
  ignore(GMisc.label ~text:"blendValuePercentageBelowHighestValueToKeep" ~packing:hbox4#add ());
  ignore(GEdit.entry ~text:"25" ~max_length:2 ~packing:hbox4#add ());
  ignore(GMisc.label ~text:"eproverTimeLimit" ~packing:hbox5#add ());
  ignore(GEdit.entry ~text:"5" ~max_length:2 ~packing:hbox5#add ());
  ignore(GMisc.label ~text:"darwinTimeLimit" ~packing:hbox6#add ());
  ignore(GEdit.entry ~text:"2" ~max_length:2 ~packing:hbox6#add ());
  ignore(GMisc.label ~text:"HDTP results" ~packing:hbox7#add ());
  ignore(GEdit.entry ~text:"3" ~max_length:2 ~packing:hbox7#add ());
  ignore(GMisc.label ~text:"Search Depth" ~packing:hbox8#add ());
  ignore(GEdit.entry ~text:"3" ~max_length:2 ~packing:hbox8#add ());
  let but = GButton.button ~label:"Exit" ~packing:vbox#pack () in
  ignore(but#connect#clicked ~callback:windowoptions#destroy);
  windowoptions#show ()
    
let call_hets s () = 
  print_endline ("hets -g "^s);
  ignore(Unix.system("hets -g "^s));()

let run path () = 
    let windowgen = GWindow.window ~title:"Example Calculated Blend" 
      ~width:340 ~height:400 ~modal:true ~border_width:10 () in
    let vvbox = GPack.vbox ~packing:windowgen#add () in
    let s1 =  (GSourceView2.source_view
    ~width:200 ~height:350
    ~packing:vvbox#add 
    ()) in
    let t = r_open_in (path^"group/rotation.dol") in
    ignore(s1#source_buffer#set_text t);
    let _ = ignore(s1#source_buffer#set_language lang) in
    let button = GButton.button ~label:"Show in HETS" ~packing:vvbox#pack () in
    let button2 = GButton.button ~label:"OK" ~packing:vvbox#pack () in
    ignore(button#connect#clicked ~callback:(call_hets (path^"group/rotation.dol")));
    ignore(button2#connect#clicked ~callback:(windowgen#destroy));
    windowgen#show ()

let group_files path = 
    [("assoc",path^"group/assoc.dol");
     ("cyclicity",path^"group/cyclicity.dol");
     ("func",path^"group/func.dol");
     ("line",path^"group/line.dol");
     ("rotation180",path^"group/rotation180.dol");
     ("binop",path^"group/binop.dol");
     ("finite",path^"group/finite.dol");
     ("ident",path^"group/ident.dol");
     ("magnitude",path^"group/magnitude.dol");
     ("comm",path^"group/comm.dol");
     ("finiteident",path^"group/finiteident.dol");
     ("inv",path^"group/inv.dol");
     ("natsuc",path^"group/natsuc.dol")]
  
let choices1 = ref []
let scrolled_window1 = ref (GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()) 
let choices2 = ref []
let scrolled_window2 = ref (GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()) 

let rec rem el l = 
  match l with
    | [] -> []
    | el2::t ->
       if (el = el2) then (rem el t)
       else el2::(rem el t)
    
let combochoices = ("axiom",["none";"both";"argument";"axiom";"custom"])


let manage path () = 
  let rec create_list_grp () =
    let selection_changed (model:#GTree.model) selection = 
      let pr path = 
	let arr = GTree.Path.get_indices path in
	arr.(0)
      in
      pr (List.hd (selection#get_selected_rows))
    in
    let on_row_activated1 t m n = 
      let arr=GTree.Path.get_indices m in
      let t = (List.nth !choices1 arr.(0)) in 
      choices1 := rem t !choices1;
      choices2 := (t,combochoices)::(!choices2);
      List.iter !scrolled_window1#remove !scrolled_window1#children;
      List.iter !scrolled_window2#remove !scrolled_window2#children;
      let (l1,l2) = create_list_grp () in
      !vpain_man#add1 l1;!vpain_man#add2 l2;
      ()
    in
    let on_row_activated2 t m n = 
      let arr=GTree.Path.get_indices m in
      let (t1,t2) = (List.nth !choices2 arr.(0)) in 
      choices2 := rem (t1,t2) !choices2;
      choices1 := t1::(!choices1);
      List.iter !scrolled_window1#remove !scrolled_window1#children;
      List.iter !scrolled_window2#remove !scrolled_window2#children;
      let (l1,l2) = create_list_grp () in
      !vpain_man#add1 l1;!vpain_man#add2 l2;
      ()
    in
    let cols_lem = new GTree.column_list in
    let str_col_lem = cols_lem#add Gobject.Data.string in
  (* Create a new scrolled window, with scrollbars only if needed *)
    let model = GTree.list_store cols_lem in
    let treeview = GTree.view ~model ~packing:(!scrolled_window1#add_with_viewport) () in
    model#clear ();
    List.iter
      (fun x -> let iter = model#append () in
		model#set ~row:iter ~column:str_col_lem (Printf.sprintf "%s" (fst x)))
      !choices1;
  (* get mutate results here *)
    let renderer = GTree.cell_renderer_text [] in
    let column = GTree.view_column ~title:"Input concepts"
      ~renderer:(renderer, ["text", str_col_lem]) () in
    treeview#selection#set_mode `SINGLE;  (* could do multiple here *)
    ignore(treeview#selection#connect#changed ~callback:(fun () -> ignore(let x = (selection_changed model treeview#selection) in !source_man#source_buffer#set_text (r_open_in (snd (List.nth !choices1 x))))));
    ignore(treeview#connect#row_activated ~callback:(fun m n -> on_row_activated1 treeview m n));
    ignore(treeview#append_column column);
    let cols_lem = new GTree.column_list in
    let name_col = cols_lem#add Gobject.Data.string in
    let value_col = cols_lem#add Gobject.Data.string in
    let domain_col = cols_lem#add (Gobject.Data.gobject_by_name "GtkTreeModel") in
  (* Create a new scrolled window, with scrollbars only if needed *)
    let name_renderer = GTree.cell_renderer_text [], ["text", name_col] in
    let string_col = (new GTree.column_list)#add Gobject.Data.string in
    let value_renderer = 
      GTree.cell_renderer_combo 
	[`VISIBLE true;`TEXT_COLUMN string_col;`HAS_ENTRY false;`EDITABLE true;] in
    let view_name_col = GTree.view_column ~title:"Concept" ~renderer:name_renderer () in
    let view_value_col = GTree.view_column ~title:"Gen Scheme" ~renderer:(value_renderer,[]) () in
    let _ =
      view_value_col#add_attribute value_renderer "model" domain_col;
      view_value_col#add_attribute value_renderer "text" value_col;
    in
    let list_store = GTree.list_store cols_lem in
    let model = (list_store :> GTree.model) in
    let treeview = GTree.view ~model ~packing:(!scrolled_window2#add_with_viewport) ~show:true() in
    let _ = treeview#append_column view_name_col in
    let _ = treeview#append_column view_value_col in
    List.iter
      (fun ((n1,n2),(v,d)) -> 
	(let iter = list_store#append ()  in
	 let d,_ = GTree.store_of_list Gobject.Data.string d in
	 list_store#set ~row:iter ~column:name_col (Printf.sprintf "%s" n1);
	 list_store#set ~row:iter ~column:value_col v;
	 list_store#set ~row:iter ~column:domain_col (d#as_model)))
      !choices2;
    treeview#selection#set_mode `SINGLE;  (* could do multiple here *)
    ignore(treeview#selection#connect#changed ~callback:(fun () -> ignore(let x = (selection_changed model treeview#selection) in !source_man#source_buffer#set_text (r_open_in (snd (fst(List.nth !choices2 x)))))));
   ignore(treeview#connect#row_activated ~callback:(fun m n -> on_row_activated2 treeview m n));
    (!scrolled_window1#coerce,!scrolled_window2#coerce)
  in
  choices1 := group_files path;
  let windowmanage = GWindow.window ~title:"Blend Theory Group" 
    ~width:515 ~height:500 ~modal:true ~border_width:10 () in
  let vbox1 = GPack.vbox ~packing:windowmanage#add () in
  let vpm = GPack.paned `VERTICAL ~packing:vbox1#add () in
  let vbl = GPack.vbox ~packing:vpm#add1 () in
  let frame_man = GBin.frame ~packing:vbl#pack () in
  vpain_man := GPack.paned `HORIZONTAL ~packing:vbl#add ();
  let bbox_man = GPack.button_box `HORIZONTAL ~border_width:15 ~layout:`SPREAD
    ~height:50 ~child_height:20 ~child_width:50 ~spacing:20 ~packing:frame_man#add () in
  let button_options = GButton.button ~packing:bbox_man#add () in
  let button_run = GButton.button ~packing:bbox_man#add  () in
  let button_quit = GButton.button ~packing:bbox_man#add  ()  in
  ignore(xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Options" ~packing:button_options#add ());
  ignore(xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Run" ~packing:button_run#add ());
  ignore(xpm_label_box ~file:(path^"/coinvent.xpm") ~text:"Exit" ~packing:button_quit#add ());
  ignore(button_options#connect#clicked ~callback:options);
  ignore(button_run#connect#clicked ~callback:(fun () -> run path ()));
  ignore(button_quit#connect#clicked ~callback:windowmanage#destroy);
  vpm#add1 vbl#coerce;
  vpm#add2 !scrolled_man#coerce;
  vpm#set_position 200;
  let (l1,l2) = create_list_grp () in !vpain_man#add1 l1;!vpain_man#add2 l2;
  !vpain_man#set_position 120;
  windowmanage#show ()
