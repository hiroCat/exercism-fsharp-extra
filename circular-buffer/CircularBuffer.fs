module CircularBuffer

type buffer =
    {
        size :int;
        elements : int seq 
    }
let lenB b = b.elements |> Seq.length

let addB v b = {b with elements = Seq.append b.elements [v]}



let mkCircularBuffer size = {size=size; elements=Seq.empty;}

let clear buffer = {buffer with elements = Seq.empty}

let write value buffer = if (buffer|>lenB) < buffer.size then 
                            buffer |> addB value
                         else 
                            failwith "the buffer is full"
        
let forceWrite value buffer = if (buffer|>lenB) < buffer.size then 
                                buffer |> addB value
                              else 
                                {buffer with elements = Seq.append (buffer.elements |> Seq.tail)[value]}

let read buffer = if (buffer|>lenB) > 0 then 
                    ((buffer.elements |> Seq.head),{buffer with elements = buffer.elements |> Seq.tail})
                  else 
                    failwith "No elements here man "