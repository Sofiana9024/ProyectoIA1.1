namespace CSP
open Capitulo3
open Busqueda
open System
open CSP

module Sudoku =

    type Variable = int * int // (row, col)
    type Value = int

    // Define the Sudoku problem as a CSP
    let createSudokuCSP (initialBoard: int[,]) =
        // Variables are all empty cells (where value is 0)
        let variables = 
            [ for row in 0..8 do 
                for col in 0..8 do 
                    if initialBoard.[row,col] = 0 then 
                        yield (row,col) ]

        // Domains for empty cells (1-9), fixed cells get their fixed value
        let dominios = 
            variables |> List.map (fun _ -> [1..9])

        // Get all peers (cells in same row, column, or box)
        let getPeers (row, col) =
            let sameRow = [ for c in 0..8 do if c <> col then yield (row, c) ]
            let sameCol = [ for r in 0..8 do if r <> row then yield (r, col) ]
            let startRow = row / 3 * 3
            let startCol = col / 3 * 3
            let sameBox = 
                [ for r in startRow..startRow+2 do
                    for c in startCol..startCol+2 do
                        if (r,c) <> (row,col) then yield (r,c) ]
            sameRow @ sameCol @ sameBox |> List.distinct

        // Create constraints
        let restricciones =
            [ for (row, col) in variables do
                // Get all peers that are either fixed or variables
                for (r, c) in getPeers (row, col) do
                    if initialBoard.[r,c] <> 0 then
                        // Constraint with fixed cell
                        yield NAria ([(row,col)], 
                            (fun estado -> 
                                List.head estado.[(row,col)] <> initialBoard.[r,c]))
                    else
                        // Constraint with variable cell
                        yield Binaria (((row,col), (r,c)), 
                            (fun estado -> 
                                List.head estado.[(row,col)] <> List.head estado.[(r,c)]))
            ]

        { variables = variables
          dominios = dominios
          restricciones = restricciones }

    // Print the Sudoku board
    let printBoard (board: int[,]) =
        for row in 0..8 do
            for col in 0..8 do
                let value = board.[row,col]
                if value = 0 then printf ". "
                else printf "%d " value
            printfn ""
        printfn ""

    // Convert solution (Map of assignments) back to a 2D array
    let solutionToBoard (initialBoard: int[,]) (solution: Map<Variable, Value list>) =
        let board = Array2D.copy initialBoard
        for KeyValue((row,col), values) in solution do
            board.[row,col] <- List.head values
        board

    // Example Sudoku boards
    let tabla1 = array2D [
        [6; 0; 8; 7; 0; 2; 1; 0; 0]
        [4; 0; 0; 0; 1; 0; 0; 0; 2]
        [0; 2; 5; 4; 0; 0; 0; 0; 0]
        [7; 0; 1; 0; 8; 0; 4; 0; 5]
        [0; 8; 0; 0; 0; 0; 0; 7; 0]
        [5; 0; 9; 0; 6; 0; 3; 0; 1]
        [0; 0; 0; 0; 0; 6; 7; 5; 0]
        [2; 0; 0; 0; 9; 0; 0; 0; 8]
        [0; 0; 6; 8; 0; 5; 2; 0; 3]

    ]

    let tabla2 = array2D [
        [0; 7; 0; 0; 4; 2; 0; 0; 0]
        [0; 0; 0; 0; 0; 8; 6; 1; 0]
        [3; 9; 0; 0; 0; 0; 0; 0; 7]
        [0; 0; 0; 0; 0; 4; 0; 0; 9]
        [0; 0; 3; 0; 0; 0; 7; 0; 0]
        [5; 0; 0; 1; 0; 0; 0; 0; 0]
        [8; 0; 0; 0; 0; 0; 0; 7; 6]
        [0; 5; 4; 8; 0; 0; 0; 0; 0]
        [0; 0; 0; 6; 1; 0; 0; 5; 0]
    ]

    [<EntryPoint>]
    let main argv =
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
        printBoard tabla

        // Create CSP
        let csp = createSudokuCSP tabla
        printfn "Variables: %d" csp.variables.Length
        printfn "Restricciones: %d" csp.restricciones.Length

        // Solve with backtracking
        match CSP.backtracking csp with
        | (stats, Some solutionNode) ->
            printfn "Nodos generados: %d" stats.nodos_generados
            printfn "Nodos procesados: %d" stats.nodos_procesados
            
            let solutionState = solutionNode.estado
            let solvedBoard = solutionToBoard tabla solutionState
            printfn "\nSudoku resuelto:"
            printBoard solvedBoard
            0
        | (stats, None) ->
            printfn "Nodos generados: %d" stats.nodos_generados
            printfn "Nodos procesados: %d" stats.nodos_procesados
            printfn "No existe solución :("
            1