val run :
     ?file:string
  -> config:string
  -> output:string
  -> unit
  -> (Fpath.t list, [> `Msg of string ]) Result.t
