(**
   Module MidiV
*)

(**
   Standard 44,1kHz samplerate
*)
let samplerate : int = 44100

(**
   Standard MIDI-division value
 *)
let div_value = 96
let division : MIDI.division = MIDI.Ticks_per_quarter div_value

(**
   Default number of channels to use
 *)
let channels : int = 16

(**
   Convert a 7 bit integer between 0 and 127 to a float
*) 
let velocityFromInt : int -> float =
  fun x ->
    let trim x = max 0. (min x 1.) in
    trim ((float_of_int x) /. 127.)

(** Code courtesy of the savonet project
   @author "D. Baelde" "S. Mimram" *)

type delta = int

(* Tempo is in microseconds per quarter. *)
let samples_of_delta samplerate division tempo delta =
  match division with
  | MIDI.Ticks_per_quarter tpq ->
     (* These computations sometimes overflow on 32 bits. *)
     let tpq = Int64.of_int tpq in
     let tempo = Int64.of_int tempo in
     let tps = Int64.of_int samplerate in
     let microseconds_to_seconds = Int64.of_int 1000000 in
     let delta = Int64.of_int delta in
     let ( * ) = Int64.mul in
     let ( / ) = Int64.div in
     let return_val =
       ((((delta * tempo) / tpq) * tps) / microseconds_to_seconds)
     in
     Int64.to_int return_val
  | MIDI.SMPTE (fps,res) ->
     (samplerate * delta) / (fps * res)

let timeToSamplesNumber : ?samplerate:int -> ?division:MIDI.division ->
			  ?tempo:Time.Tempo.t -> duration:Time.t -> int =
  fun ?samplerate:(sr = samplerate) ?division:(div = division)
      ?tempo:(tempo = Time.Tempo.base)
      ~duration ->
  let tempo_mspq = Time.Tempo.toMicrosecondsPerQuarters tempo in
  samples_of_delta sr div tempo_mspq (Time.toMidiTicks div duration)

let samplesToSeconds ?samplerate:(samplerate = samplerate) samples =
  let duration_seconds_num =
    Num.mult_num (Num.num_of_int samples) @@
      Num.div_num (Num.num_of_int 1) (Num.num_of_int samplerate)
  in
  Num.float_of_num duration_seconds_num
