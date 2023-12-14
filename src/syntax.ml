let ( let*! ) r f = match r with Error (`Msg e) -> failwith e | Ok v -> f v
let ( let* ) = Result.bind
let ( >>| ) lst f = List.map f lst
let ( >>= ) lst f = List.concat_map f lst

let list_map f lst =
  let exception E of string in
  try
    Ok
      (List.map
         (fun x -> match f x with Ok x -> x | Error e -> raise (E e))
         lst )
  with E e -> Error e
