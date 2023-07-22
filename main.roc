app "brainroc"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        # pf.File,
        # pf.Path,
        pf.Stdout,
        # pf.Task,
    ]
    provides [main] to pf

main =
  Stdout.line "Hello, world!"

tokenize : List U8 -> List Op
tokenize = \bytes ->
  beginState = (0, [], [])
  (_pc, instructions, _jumpStack) = List.walk bytes beginState tokenizeOne
  instructions

tokenizeOne = \(pc, instructions, jumpStack), char ->
  when char is
    '>' ->
      instructions2 = List.append instructions Next
      (pc + 1, instructions2, jumpStack)

    '<' ->
      instructions2 = List.append instructions Prev
      (pc + 1, instructions2, jumpStack)

    '+' ->
      instructions2 = List.append instructions Inc
      (pc + 1, instructions2, jumpStack)

    '-' ->
      instructions2 = List.append instructions Dec
      (pc + 1, instructions2, jumpStack)

    '.' ->
      instructions2 = List.append instructions Output
      (pc + 1, instructions2, jumpStack)

    ',' ->
      instructions2 = List.append instructions Input
      (pc + 1, instructions2, jumpStack)

    '[' ->
      instructions2 = List.append instructions (JumpForward 0)
      jumpStack2 = List.append jumpStack pc
      (pc + 1, instructions2, jumpStack2)

    ']' ->
      when pop jumpStack is
        Err _ -> crash "Malformed BF program"
        Ok (location, jumpStack2) ->
          instructions2 =
            instructions
            |> List.append (JumpBackward location)
            |> List.update location (\op -> patchJump pc op)
          (pc + 1, instructions2, jumpStack2)

    ' ' | '\n' | '\t' ->
      (pc, instructions, jumpStack)

    other ->
      when Str.fromUtf8 [ other ] is
          Ok str -> 
              crash "unexpected character: '\(str)'" 

          Err _ -> 
              crash "unexpected non-utf8 character"  

patchJump : Nat, Op -> Op
patchJump = \targetLocation, op ->
  when op is
    JumpForward _ ->
      JumpForward targetLocation
    _ ->
      crash "Incorrect jump patch"

Op : [
  Next,
  Prev,
  Inc,
  Dec,
  Input,
  Output,
  JumpForward Nat,
  JumpBackward Nat,
]


State : {
  program: List Op,
  programCounter: Nat,
  data: List U8,
  dataCounter: Nat,
  output: List U8,
}

dataSize = 1_000

initialState : List Op -> State
initialState = \program ->
  {
    program: program,
    programCounter: 0,
    data: (List.repeat 0 dataSize),
    dataCounter: 0,
    output: [],
  }

runOne : State -> Result State [Done State]
runOne = \state ->
  if state.programCounter > List.len state.program then
    Err (Done state)
  else
    op = getUnsafe state.program state.programCounter
    state2 =
      when op is
        Next ->
          {state & dataCounter: state.dataCounter + 1}
        Prev ->
          {state & dataCounter: state.dataCounter - 1}
        Inc ->
          data = List.update state.data state.dataCounter (\x -> x + 1)
          {state & data}
        Dec ->
          data = List.update state.data state.dataCounter (\x -> x - 1)
          {state & data}
        Input ->
          crash "Input (,) is not implemented yet"
        Output ->
          val = getUnsafe state.data state.dataCounter
          output2 = List.append state.output val
          {state & output: output2}
        JumpForward targetLocation ->
          val = getUnsafe state.data state.dataCounter
          if val == 0 then
            {state & programCounter: targetLocation}
          else
            state
        JumpBackward targetLocation ->
          val = getUnsafe state.data state.dataCounter
          if val != 0 then
            {state & programCounter: targetLocation}
          else
            state
          
    Ok state2

run : State -> List U8
run = \state ->
  when runOne state is
    Ok state2 ->
      run state2
    Err (Done state2) ->
      state2.output

pop : List a -> Result (a, List a) [ListWasEmpty]
pop = \list ->
  when List.last list is
    Err x -> Err x
    Ok elem ->
      rest = List.dropLast list
      Ok (elem, rest)

getUnsafe : List a, Nat -> a
getUnsafe = \list, index ->
  when List.get list index is
    Err OutOfBounds ->
      crash "Out of bounds"
    Ok val ->
      val