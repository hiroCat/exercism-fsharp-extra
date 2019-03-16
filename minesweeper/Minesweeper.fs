module Minesweeper

open System

type position =
    {
        x:int 
        y:int
    }

let minesNumb x n = 
    x
    |> Seq.mapi(fun i x -> if x = '*' then Some({x=n;y=i;})
                           else None)
    |> Seq.choose id

let minesList x = 
    x 
    |> Seq.mapi(fun i x -> i |> minesNumb x)
    |> Seq.concat

let sorroundP (p:position) = 
    [{x = p.x-1;    y = p.y-1};
     {x = p.x-1;    y = p.y};
     {x = p.x-1;    y = p.y+1};
     {x = p.x;      y = p.y-1};
     {x = p.x;      y = p.y+1};
     {x = p.x+1;    y = p.y-1};
     {x = p.x+1;    y = p.y};
     {x = p.x+1;    y = p.y+1}] |> List.toSeq

let count m p = 
    let ps = p |> sorroundP |> Seq.filter (fun x -> m |> Seq.contains x) |> Seq.length 
    if ps = 0 then ' '
    else System.Char.Parse(ps.ToString())

let tranform i m = 
    i
    |> List.mapi(fun i g -> g 
                           |> Seq.mapi(fun n x -> if x = ' ' then {x=i;y=n;}|> count m
                                                  else x)
                           |> String.Concat )
    

let annotate (input:string list) =  
    let mines = input |> minesList
    if mines |> Seq.isEmpty then input
    else mines |> tranform input
    
