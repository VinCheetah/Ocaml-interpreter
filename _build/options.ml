let debug = ref false
and showsrc = ref false
and slow = ref false
and tree = ref false
and trace = ref false
and warnings = ref false
and in_dscolon = ref false
and output = ref false
and notypes = ref false
and showtypes = ref false
and showinf = ref false



let actu_options () =
  if !debug then (showsrc := true; tree := true; trace := true; warnings := true; output := true; showinf := true)
  else if !showinf then showtypes := true;