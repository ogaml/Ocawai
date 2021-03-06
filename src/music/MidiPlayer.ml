open Synth
open Instrument

module MusicLog = Log.Make (struct let section = "Music" end) 

let might_adsr adsr g =
  match adsr with
    | None -> g
    | Some a -> new Audio.Mono.Generator.adsr a g

let volume = ref 15

let set_volume v = volume := v
let get_volume () = !volume

class asynchronousMidiPlayer =

  let channels = 2 in
  let sample_rate = 44100 in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let mchannels = 16 in
  let mbuf = MIDI.Multitrack.create mchannels blen in
  
  object(self)

    val mutable main_buffer = ref (MIDI.Multitrack.create mchannels 100000)
    val mutable should_run = ref true
    val mutable current_playing = ref 0
    val mutable current_adding = ref 0
    val synth =
      let synth = fun () -> to_synth ~samplerate:sample_rate Organ in
      new Synth.Multitrack.create mchannels @@
	(function 
	  | 0 -> to_synth ~samplerate:sample_rate Kick
	  | 1 -> to_synth ~samplerate:sample_rate Snare
	  | _ -> synth ())
	  
    (* This is just blit mapped in every sub channel of a multitrack buf *)
    method private multi_blit b1 o1 b2 o2 len =
      if (MIDI.Multitrack.channels b1 = MIDI.Multitrack.channels b2) then begin
        for i = 0 to (MIDI.Multitrack.channels b1) - 1 do
          MIDI.blit (b1.(i)) o1 (b2.(i)) o2 len
        done
      end else failwith "Wrong number of channels"

    (* This is just add mapped in every sub channel of a multitrack buf *)
    method private multi_add b1 o1 b2 o2 len =
      if (MIDI.Multitrack.channels b1 = MIDI.Multitrack.channels b2) then begin
        for i = 0 to (MIDI.Multitrack.channels b1) - 1 do
          MIDI.add b1.(i) o1 b2.(i) o2 len
        done
      end else failwith "Wrong number of channels"

    method play () =
      let pulse = new MMPulseaudio.writer
                        "OCAWAI"
                        "Music of the OCAWAI game"
                        channels
                        sample_rate in
      let () = MusicLog.info "Started playing music, created pulseaudio output" in
      while (!should_run) do
        let agc = Audio.Effect.auto_gain_control channels sample_rate ~volume_init:((float_of_int !volume) /. 100.) () in
        self#multi_blit (!main_buffer) (!current_playing) mbuf 0 blen;
        current_playing := !current_playing + blen + 1;
        synth#play mbuf 0 buf 0 blen;
        agc#process buf 0 blen;
        pulse#write buf 0 blen
      done;
      pulse#close;
      MusicLog.info "Closed pulseaudio output"

    method stop () =
      MusicLog.info "Stopping Music playback";
      should_run := false

    method add new_buffer = 
      self#multi_add (!main_buffer) (!current_adding) new_buffer 0
		     (MIDI.Multitrack.duration new_buffer);
      current_adding := !current_adding + (MIDI.Multitrack.duration new_buffer)

    method remaining () =
      (!current_adding - !current_playing)

  end

let play_midi_file fname run =
  let blen = 1024 in
  let sample_rate = 44100 in
  let f = ref (new MIDI.IO.Reader.of_file fname) in
  let player = new asynchronousMidiPlayer in
  let tmpbuf = MIDI.Multitrack.create 16 blen in
  let r = ref (!f#read sample_rate tmpbuf 0 blen) in
  player#add tmpbuf;
  let _ = Thread.create (player#play) () in
  while !run do
    Thread.delay 0.001;
    if (player#remaining ()) < 2048 then begin
      !f#close;
      f := new MIDI.IO.Reader.of_file fname;
      r := !f#read sample_rate tmpbuf 0 blen;
      player#add tmpbuf
    end;
    while !r <> 0 do
      r := !f#read sample_rate tmpbuf 0 blen;
      player#add tmpbuf
    done;
    while !r = 0 && (player#remaining ()) < 2048 do
      Thread.delay 0.2
    done
  done;
  !f#close;
  player#stop ();
  Thread.exit ()
