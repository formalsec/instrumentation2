val run :
     ?file:string
  -> config:string
  -> output:string
  -> unit
  -> (Fpath.t list, [> Result.err ]) Result.t
