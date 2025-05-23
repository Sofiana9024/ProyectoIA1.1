open System
open Busqueda
open Capitulo3
open Utils
open CSP
open SudokuUtils
open SudokuCSP

[<EntryPoint>]
let main argv =
    let tabla1 = array2D [
        [6; 0; 8; 7; 0; 2; 1; 0; 0];
        [4; 0; 0; 0; 1; 0; 0; 0; 2];
        [0; 2; 5; 4; 0; 0; 0; 0; 0];
        [7; 0; 1; 0; 8; 0; 4; 0; 5];
        [0; 8; 0; 0; 0; 0; 0; 7; 0];
        [5; 0; 9; 0; 6; 0; 3; 0; 1];
        [0; 0; 0; 0; 0; 6; 7; 5; 0];
        [2; 0; 0; 0; 9; 0; 0; 0; 8];
        [0; 0; 6; 8; 0; 5; 2; 0; 3]
    ]

    let tabla2 = array2D [
        [0; 7; 0; 0; 4; 2; 0; 0; 0];
        [0; 0; 0; 0; 0; 8; 6; 1; 0];
        [3; 9; 0; 0; 0; 0; 0; 0; 7];
        [0; 0; 0; 0; 0; 4; 0; 0; 9];
        [0; 0; 3; 0; 0; 0; 7; 0; 0];
        [5; 0; 0; 1; 0; 0; 0; 0; 0];
        [8; 0; 0; 0; 0; 0; 0; 7; 6];
        [0; 5; 4; 8; 0; 0; 0; 0; 0];
        [0; 0; 0; 6; 1; 0; 0; 5; 0]
    ]

    printfn "Seleccione una opción de sudoku:\n1. Ejemplo 1\n2. Ejemplo 2"
    let choice = Console.ReadLine()

    let tabla =
        match choice with
        | "1" -> tabla1
        | "2" -> tabla2
        | _ -> 
            printfn "Opción inválida, se usará el 1"
            tabla1

    printfn "\nSudoku sin resolver:"
    printTabla tabla

    let csp = construirSudokuCSP tabla
    let estadistica, resultado = CSP.backtracking csp

    match resultado with
    | Some nodoFinal ->
        let solucion = nodoFinal.estado
        for fila in 0 .. 8 do
            for col in 0 .. 8 do
                match Map.tryFind (fila, col) solucion with
                | Some [valor] -> tabla.[fila, col] <- valor
                | _ -> ()
        printfn "Sudoku resuelto:"
        printTabla tabla
    | None ->
        printfn "No existe solución :("

    0
