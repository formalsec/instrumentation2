module Result = struct
  let ( let* ) v f = Result.bind v f
  let ( let+ ) v f = Result.map f v

  let list_map f lst =
    let exception Exit in
    let err = ref None in
    try
      Ok
        (List.map
           (fun x ->
             match f x with
             | Ok x -> x
             | Error e ->
               err := Some e;
               raise_notrace Exit )
           lst )
    with Exit -> Error (Option.get !err)
end

module List = struct
  let ( let* ) v f = List.concat_map f v
  let ( let+ ) v f = List.map f v
end
