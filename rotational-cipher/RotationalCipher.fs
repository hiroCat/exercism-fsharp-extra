module RotationalCipher
open System

let alphabet = ['A';'B';'C';'D';'E';'F';'G';'H';'I';
                'J';'K';'L';'M';'N';'O';'P';'Q';'R';
                'S';'T';'U';'V';'W';'X';'Y';'Z']

let numb a = alphabet 
                |> List.tryFindIndex(fun e -> e = (a|>Char.ToUpper))

let format o d =  o 
                  |> Char.IsUpper  
                  |> function true ->  d |> Char.ToUpper
                              |false -> d |> Char.ToLower

let letter i = alphabet.[i%26]

let rotate (shiftKey:int) text  = 
    text 
    |> Seq.map(fun x -> numb x 
                        |> function Some n -> letter (n+shiftKey)
                                              |> format x  
                                    |None -> x)

    |> String.Concat
