(* ::Package:: *)

Package["Project`"];

(*Define what methods are visible*)
PackageExport[EncodeTrack];
PackageExport[ToSound];


(* ::CodeText:: *)
(*Constants*)


TimeShiftStepDuration = 10; (*ms*)
TimeMeasure = 1000 ;(* one second 1*)

NoteOnByte = "9";
NoteOffByte = "8";
MetaMessageByte = "F";

NoteOnEvent = "NoteOn";
NoteOffEvent = "NoteOff";
TimeShiftEvent = "TimeShift";
VelocityEvent = "VelocityChange";

MidiMaxValue = 127;
MidiNoteShift = 20; (*We use only 88 notes what is real piano range. So we need to subtract from MIDI number*)

NoteEventsLength = 88;
TimeShiftsLength = 100;
MaxTimeShift = (TimeShiftStepDuration / TimeMeasure ) * TimeShiftsLength;
VelocitiesLength = 34;
VelocityQuantization = VelocitiesLength / MidiMaxValue;

NoteOnEventsEnd = NoteEventsLength;
NoteOffEventsEnd = NoteEventsLength * 2;
TimeShiftsEnd = NoteOffEventsEnd + TimeShiftsLength;
VelocitiesEnd = TimeShiftsEnd + VelocitiesLength;


(* ::CodeText:: *)
(*Utilities*)


(* ::Input::Initialization:: *)
(*Create one hot vector of index*)
OneHotEncoding[index_Integer] := If[True, index, UnitVector[VelocitiesEnd, index]];

EncodeNote[midiNote_Integer, isOn_] := Block[{note = midiNote - MidiNoteShift},
  If[note <= 0, note = midiNote + 24(* Adding missing two octaves just in case *) - MidiNoteShift];

  OneHotEncoding[If[isOn, note, NoteOnEventsEnd + note]]
];

DecodeNote[position_] := If[position < NoteEventsLength, position + MidiNoteShift, position - NoteEventsLength + MidiNoteShift];

EncodeVelocity[velocity_Integer] := OneHotEncoding[TimeShiftsEnd + QuantizedVelocity[velocity]];

QuantizedVelocity[velocity_] := Floor[VelocityQuantization * velocity];

DecodeVelocity[position_] := Floor[N[(position - TimeShiftsEnd ) / VelocityQuantization]];

MaxTimeShiftEncodings[count_] := Table[OneHotEncoding[ToTimeShiftIndex[MaxTimeShift]], {i, count }];

EncodeTimeShift[duration_Real] := If[duration <= MaxTimeShift,
(* Create only one vector with duration less or equal to one second *)
  List[OneHotEncoding[ToTimeShiftIndex[duration]]],

(* Otherwise create a list of one hot vectors*)
  If[FractionalPart[duration] > 0,
    Join[
      MaxTimeShiftEncodings[IntegerPart[duration]], EncodeTimeShift[FractionalPart[duration]]
    ],

    MaxTimeShiftEncodings[IntegerPart[duration]]
  ]
];

ToTimeShiftIndex[duration_] := Block[{index},
  index = NoteOffEventsEnd + Round[(duration * TimeMeasure) / TimeShiftStepDuration];
  If[index == NoteOffEventsEnd, index = NoteOffEventsEnd + 1, index]
];

DecodeTimeShift[position_] := N[((position - NoteOffEventsEnd) * TimeShiftStepDuration) / TimeMeasure]; (* return seconds *)

StatusByte[note_] := note[[2, 1]];(* status byte position *)

VelocityByte[note_] := note[[3, 2]];(* velocity byte position *)

NoteByte[note_] := note[[3, 1]];(* midi note byte position *)

TimeShiftByte[note_, secondsPerTick_] := note[[1]] * secondsPerTick;(* time shift byte position *)

NoteEvents[raw_, secondsPerTick_] := Select[#,
  StatusByte[#] == NoteOnByte
      || StatusByte[#] == NoteOffByte
      || (TimeShiftByte[#, secondsPerTick] > (TimeShiftStepDuration / TimeMeasure) && StatusByte[#] != MetaMessageByte)&
] & /@ raw;

TrimTracks[noteEvents_] := Cases[noteEvents, Except[{}]];

GetMidiEvents[raw_, secondsPerTick_] := TrimTracks[NoteEvents[raw, secondsPerTick]][[1]]; (* We are looking for a first track of midi tracks *)

RangeToNote = AssociationThread[Range[0, 11] -> {"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"}];

NoteToRange = AssociationMap[Reverse, RangeToNote];

IdToNote[id_] := With[{d = id - 60}, RangeToNote@Mod[d, 12] <> ToString[Floor[d / 12] + 4]];


(* ::Input::Initialization:: *)
GetTimePositions[track_,seconds_, secondsPerTick_]:=Block[{positions ={},time =0},
Do[
time = time + track[[i]][[1]] * secondsPerTick;
If[time > seconds, positions= Append[positions,i]; time =0;],

{i, Length@track}];

positions
]


(* ::CodeText:: *)
(*Encode midi sequence:*)


EncodeMidi[track_, secondsPerTick_] := Block[{lastVelocity = 0},
  ClearAll[list];
  Flatten[
    Map[
      Block[{list = {}},
      (* Add time shifts when needed *)
        If[TimeShiftByte[#, secondsPerTick] > 0, list = Join[list, EncodeTimeShift[TimeShiftByte[#, secondsPerTick]]]];

        (* Proceed with logic only if it's a note event *)
        If[StatusByte[#] == NoteOnByte || StatusByte[#] == NoteOffByte,

        (* Add velocity if it's different from the last seen *)
          If[lastVelocity != QuantizedVelocity[VelocityByte[#]] && StatusByte[#] == NoteOnByte,

            lastVelocity = QuantizedVelocity[VelocityByte[#]];
            list = Join[list, List[EncodeVelocity[VelocityByte[#]]]];
          ];

          (* Add note event *)
          list = Join[list, List[EncodeNote[NoteByte[#], StatusByte[#] == NoteOnByte]]];
        ];

        (* Return encoded list*)
        list
      ]&,
      track]
    , 1]];


EncodeTrack[path_] := Block[{encodings},
	{raw, header} = Import[path, #]& /@ {"RawData", "Header"};

	tempos = Cases[Flatten[raw], HoldPattern["SetTempo" -> tempo_] :> tempo];

	microsecondsPerBeat = If[Length@tempos > 0, First[tempos], 500000]; (* If there is no explicit tempo we use default 120 bpm *)
	
	timeDivision = First@Cases[header, HoldPattern["TimeDivision" -> division_] :> division];
	timeDivisionBits = IntegerDigits[timeDivision, 2];
	timeDivisionBits = If[Length@timeDivisionBits < 16, PadLeft[timeDivisionBits, 16], timeDivisionBits];
	timeDivisionType = timeDivisionBits[[1]];
	framesPerSecond = timeDivisionBits[[2;; 8]];
	ticksPerFrame = timeDivisionBits[[9;; 16]];
	
	ticksPerBeat = If[timeDivisionType == 0, timeDivision, 10^6 /(framesPerSecond * ticksPerFrame)];
	
	secondsPerTick = (microsecondsPerBeat / ticksPerBeat) * 10^-6.;
	 
	encodings = Partition[EncodeMidi[GetMidiEvents[raw, secondsPerTick], secondsPerTick], 500];
	
	Print[StringJoin["Encoded: ", path]];
	
	encodings
];


(* ::CodeText:: *)
(*Decode one-hot sequence:*)


ToEvents[encodings_, indices_] := Map[
  Block[{pos, type, value},
    pos = If[indices, #, First@Flatten[Position[#, 1]]];

    type = Which[
      pos <= NoteEventsLength, NoteOnEvent,
      pos <= NoteOffEventsEnd, NoteOffEvent,
      pos <= TimeShiftsEnd, TimeShiftEvent,
      True, VelocityEvent
    ];

    value = Switch[type,
      NoteOnEvent, DecodeNote[pos],
      NoteOffEvent, DecodeNote[pos],
      TimeShiftEvent, DecodeTimeShift[pos],
      VelocityEvent, DecodeVelocity[pos]
    ];

    type -> value
  ]&,
  encodings];


ToSound[encodings_] := Sound[SortBy[Flatten[Block[{notes, events},
  events = ToEvents[encodings, True];

  notes = Intersection[Cases[events, HoldPattern["NoteOn" -> note_] :> note], Cases[events, HoldPattern["NoteOff" -> note_] :> note]];

  Map[
    Block[{note = #, ons, offs, onOffs, durations, starts, velocities},
      ons = Flatten[Position[events, NoteOnEvent -> note]];
      offs = Flatten[Position[events, NoteOffEvent -> note]];

      Do[
        If[i <= Length@offs,
          If[ons[[i]] > offs[[i]], offs = Drop[offs, 1]];
        ]
        , {i, Length@ons}];

      If[Length@ons < Length@offs, offs = Take[offs, Length@ons], ons = Take[ons, Length@offs]];

      onOffs = Transpose @ {ons, offs};
      durations = Total @ Cases[events[[First@# + 1 ;; Last@# - 1]], HoldPattern["TimeShift" -> duration_] :> duration] & /@ onOffs;
      starts = Total @ Cases[Take[events, First@#], HoldPattern["TimeShift" -> start_] :> start] & /@ onOffs;
      velocities = Last @ Cases[Take[events, First@#], HoldPattern["VelocityChange" -> velocity_] :> velocity] & /@ onOffs;

      Table[
        Block[{start = starts[[i]], duration = durations[[i]], velocity = velocities[[i]]},

          SoundNote[IdToNote@note, {start, start + duration}, "Piano", SoundVolume -> velocity]
        ],
        {i, Length@starts}]
    ] &, notes]
]], #[[2, 1]]&]];
