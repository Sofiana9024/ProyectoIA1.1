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

(*open System
open Busqueda
open Capitulo3
open Utils
open CSP

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

let construirSudokuCSP (tabla: int[,]) =
    let todasLasPosiciones =
        [ for fila in 0 .. 8 do for col in 0 .. 8 -> (fila, col) ]

    let dominioPorDefecto = [1..9]

    // Primero definimos el mapa de dominios
    let dominiosMap =
        todasLasPosiciones
        |> List.map (fun (fila, col) ->
            let valor = tabla.[fila, col]
            if valor = 0 then ((fila, col), dominioPorDefecto)
            else ((fila, col), [valor])
        )
        |> Map.ofList

    // Luego convertimos ese Map a una lista de listas, respetando el orden
    let dominiosLista : int list list =
        todasLasPosiciones
        |> List.map (fun pos ->
            match Map.tryFind pos dominiosMap with
            | Some valores -> valores
            | None -> []  // puede que nunca ocurra, pero es seguro
        )

    // Finalmente definimos las restricciones
    let restricciones =
        [ for v in todasLasPosiciones do
            for v2 in vecinos v do
                if v < v2 then
                    CSP.Binaria((v, v2), fun estado ->
                        match Map.tryFind v estado, Map.tryFind v2 estado with
                        | Some [x], Some [y] -> x <> y
                        | _ -> true
                    )
        ]

    {
        CSP.variables = todasLasPosiciones
        CSP.dominios = dominiosLista
        CSP.restricciones = restricciones
    }


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
    let resultado = CSP.backtracking csp


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

    0*)
