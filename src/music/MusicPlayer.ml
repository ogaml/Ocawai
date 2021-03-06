(**
   Music reading and playback object
*)

open Time
open Music
open DList
open TPTM

exception Incorrect_value_chord_selection

let dummy_event_plus dur (pitch, velocity) =
  note dur (new Music.param pitch velocity)

let dummy_simple_event (dur, pitch) =
  dummy_event_plus dur (pitch, 127)

(**
   notes : [(int, Music.pitch)], duration and pitch
*) 
let sequence notes =
  let aggregate tile note =
    tile % (TPTM.make_withDelay (dummy_simple_event note))
  in
  List.fold_left aggregate TPTM.zero notes

let chord duration pitches =
  let aggregate tile pitch =
    fork tile @@ TPTM.make_withDelay (dummy_simple_event (duration, pitch))
  in
  List.fold_left aggregate TPTM.zero pitches
 
let kick_all_4 = modify (Modify.Instrument (Instrument.Kick)) @@
		   sequence [(qn, (C, 2)); (qn, (C, 2));
			     (qn, (C, 2)); (qn, (C, 2))]
let snare_2_3 = modify (Modify.Instrument (Instrument.Snare)) @@
		   (reset @@ delay qn % (sequence [(hn, (C, 5)); (qn, (C, 5))]) )
		     % delay wn

let the_beat = fork kick_all_4 snare_2_3
(*
let kick_all_4PlusOne = modify (Modify.Instrument (Instrument.Kick)) @@
			  fork (sequence [(qn, (C, 2)); (qn, (C, 2));
					  (qn, (C, 2)); (qn, (C, 2))])
			  @@ delay hn % delay qn % delay en % sequence [(en, (C, 2))]
 *)
let menu_music = fork (fork (fork ((chord hn [(C, 2); (G, 2)]) % (chord hn [(E, 2); (C, 3)]))
			     @@ sequence [(en, (C, 4)); (en, (E, 4));
					  (tren, (C, 4)); (tren, (F, 4)); (tren, (A, 5));
					  (en, (C, 5)); (en, (G, 4));
				    (en, (F, 4)); (en, (E, 4))]
			    ) @@
			 modify (Modify.Instrument (Instrument.Snare)) @@
			   (delay en) %
			     sequence [(en, (C, 5));
				       (tren, (C, 5)); (tren, (C, 5)); (tren, (C, 5));
				       (en, (C, 5)); (en, (C, 5));
				       (en, (C, 5)); (en, (C, 5))]
		      ) @@ kick_all_4

let menu_music_simple = fork ((chord hn [(C, 2); (G, 2)]) % (chord hn [(E, 2); (C, 3)]))
			@@ sequence [(en, (C, 4)); (en, (E, 4));
				     (tren, (C, 4)); (tren, (F, 4)); (tren, (A, 5));
				     (en, (C, 5)); (en, (G, 4));
				     (en, (F, 4)); (en, (E, 4))]
				 
let winner_music = function
  | 0 -> chord wn @@ [(C, 4); (E, 4); (G, 4)]
  | 1 -> menu_music_simple (* chord wn @@ [(G, 3); (C, 4); (E, 4)] *)
  | 2 -> chord hn [(C, 4); (G, 4)] % chord hn [(G, 4); (C, 5); (E, 5)] 
  | _ -> raise Incorrect_value_chord_selection

let loser_music = function
  | 0 -> chord wn @@ [(C, 4); (Ef, 4); (G, 4)]
  | 1 -> chord wn @@ [(C, 4); (Ef, 4); (A, 4)]
  | 2 -> chord hn [(D, 4); (A, 4)] % chord hn [(A, 4); (C, 5); (Ef, 5)]
  | _ -> raise Incorrect_value_chord_selection

let music_player =
  fun ?samplerate:(samplerate = MidiV.samplerate) ?division:(division = MidiV.division)
      ?tempo:(tempo = Time.Tempo.base) () ->
  object (self)
	   
    (** The main Tile we will use to store the music to play,
        we maintain Pre buffer = Pos buffer = O *)
    val mutable buffer = TPTM.zero

    method private duration_one_measure tempo =
      let duration_seconds_num =
        Num.mult_num (Num.num_of_int @@ MidiV.timeToSamplesNumber Time.wn)
                     (Num.div_num (Num.num_of_int 1) (Num.num_of_int samplerate))
      in
      Num.float_of_num duration_seconds_num
				  
    method private bufferize : TPTM.t -> unit = fun t ->
      let open TPTM in
      buffer <- reset @@ buffer % t

    method play_menu : bool ref -> unit = fun run ->
      let tempo = Time.Tempo.fromInt 100 in
      let midi_player = ref None in
      while true do
        while !run do
          match !midi_player with
            | None ->
                let mp = new MidiPlayer.asynchronousMidiPlayer in
                midi_player := Some (mp);
                ignore @@ Thread.create (mp#play) ();
                ignore @@ Thread.create (self#add_next_measure tempo mp) run;
            | Some (midi_player) ->
                self#bufferize menu_music;
              Thread.delay 0.1
        done;
        (match !midi_player with | Some midi_player -> midi_player#stop () | None -> ());
        midi_player := None;
        Thread.delay 0.1
      done

    method play_game : bool ref -> unit = fun run ->
      let tempo = Time.Tempo.fromInt 100 in
      let midi_player = ref None in
      while true do
        while !run do
          match !midi_player with
            | None ->
                let mp = new MidiPlayer.asynchronousMidiPlayer in
                midi_player := Some (mp);
                ignore @@ Thread.create (mp#play) ();
                ignore @@ Thread.create (self#add_next_measure tempo mp) run;
            | Some (midi_player) ->
	       begin
		 let mood = Mood.get () in
		 let select = Random.int 3 in
		 let next_tile =
		   if mood <= 0. then
		     the_beat % loser_music select
		   else the_beat % winner_music select
		 in
		 self#bufferize next_tile;
		 Thread.delay ((self#duration_one_measure tempo) *. 0.99)
	       end
        done;
        (match !midi_player with | Some midi_player -> midi_player#stop () | None -> ());
        midi_player := None;
        Thread.delay 0.1
      done
      
    method add_next_measure = fun tempo midi_player run ->
      while !run do
        let (next_measure, rest) =
          TPTM.extract_by_time wn buffer
        in
        buffer <- reset rest;
        let new_buffer = TPTM.to_MIDI_buffer ~tempo next_measure in
	midi_player#add new_buffer;
        Thread.delay (self#duration_one_measure tempo)
      done

    method read_note : unit =
      let running = ref true in
      print_endline "\n\027[32mInput some notes (for instance C5 A4 A3 Aff2), \
		     then press ENTER\027[0m (Input <q> to quit)";
      print_endline "\027[1;31m\t=======> Be amazed ! <=======\027[0m\n\
		     (You can even let it run in the background, \
		     it won't eat all your CPU-time :) !)\n";
      while !running do
        let notes_strings = Str.split (Str.regexp " ") (read_line ()) in
	try
          let note_reader = fun str ->
            TPTM.make_withDelay @@ dummy_simple_event @@
              (wn, Music.pitch_of_string str)
          in 
          let notes = List.map note_reader notes_strings in
          List.iter self#bufferize notes
        with Not_found ->
	     match notes_strings with
	     | ["q"] -> running := false
	     | _ -> ()
      done

    initializer
      TPTM.fprintf Format.std_formatter @@ snd @@ TPTM.headTail menu_music;
      Random.self_init ()
  end
