val run :
     file:string
  -> config:string
  -> output:string
  -> (Fpath.t list, [> `Msg of string ]) Result.t
