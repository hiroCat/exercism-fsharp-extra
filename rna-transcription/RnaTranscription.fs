module RnaTranscription

open System

let toRna (dna: string): string = 
    dna 
    |> Seq.map(fun x -> match x with 
                        | 'G' -> 'C' 
                        | 'C' -> 'G'
                        | 'T' -> 'A'
                        | 'A' -> 'U')
    |> String.Concat