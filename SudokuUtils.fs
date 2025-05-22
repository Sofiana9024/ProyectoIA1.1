module SudokuUtils

let printTabla (tabla: int[,]) =
    for fila in 0 .. 8 do
        for col in 0 .. 8 do
            let valor = tabla.[fila, col]
            if valor = 0 then
                printf ". "
            else
                printf "%d " valor
        printfn ""
    printfn ""

let vecinos (fila, col) =
    let filaV = [ for c in 0 .. 8 -> (fila, c) ]
    let colV = [ for r in 0 .. 8 -> (r, col) ]
    let startR = fila / 3 * 3
    let startC = col / 3 * 3
    let subV = [ for r in startR .. startR + 2 do for c in startC .. startC + 2 -> (r, c) ]
    Set.ofList (filaV @ colV @ subV) |> Set.remove (fila, col) |> Set.toList
