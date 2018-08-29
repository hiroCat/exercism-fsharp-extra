module DifferenceOfSquares

let squareOfSum (number: int): int = 
    let sum = [1..number] |> List.sum
    pown sum 2

let sumOfSquares (number: int): int = 
    [1..number]
    |> List.sumBy(fun x ->  pown x 2)
    
let differenceOfSquares (number: int): int = 
    squareOfSum number - sumOfSquares number