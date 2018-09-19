module Grains

let square (n: int): Result<uint64,string> = 
    match n with 
    | n when n > 64 || n < 1 -> Result.Error "Invalid input"
    | _ -> Result.Ok (uint64 (2.0**((float)n-1.0)))

let total: Result<uint64,string> = 
    Result.Ok( [0..64] 
                |> Seq.sumBy(fun x -> (x 
                                        |> square 
                                        |> function Ok r -> r 
                                                    | Error r -> (uint64 0))))
                                   
 