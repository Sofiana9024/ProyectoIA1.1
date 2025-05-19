open System

// Imprime el tablero de Sudoku
let printGrid (grid: int[,]) =
    for row in 0 .. 8 do
        for col in 0 .. 8 do
            let value = grid.[row, col]
            if value = 0 then
                printf ". "
            else
                printf "%d " value
        printfn ""
    printfn ""

// Verifica si es válido colocar un número en una celda específica
let isValid (grid: int[,]) row col num =
    // Fila
    if Array.exists (fun x -> x = num) [| for c in 0 .. 8 -> grid.[row, c] |] then false
    // Columna
    elif Array.exists (fun x -> x = num) [| for r in 0 .. 8 -> grid.[r, col] |] then false
    // Subcuadro 3x3
    else
        let startRow = row / 3 * 3
        let startCol = col / 3 * 3
        let mutable found = false
        for r in startRow .. startRow + 2 do
            for c in startCol .. startCol + 2 do
                if grid.[r, c] = num then found <- true
        not found

// Encuentra una celda vacía
let findEmptyLocation (grid: int[,]) =
    let mutable found = None
    let mutable doneLoop = false
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            if grid.[r, c] = 0 && not doneLoop then
                found <- Some(r, c)
                doneLoop <- true
    found

// Resuelve el Sudoku usando backtracking
let rec solve (grid: int[,]) =
    match findEmptyLocation grid with
    | None -> true
    | Some (row, col) ->
        [1..9]
        |> List.exists (fun num ->
            if isValid grid row col num then
                grid.[row, col] <- num
                if solve grid then true
                else
                    grid.[row, col] <- 0
                    false
            else false)

// Función principal
[<EntryPoint>]
let main argv =
    let grid1 = array2D [
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

    let grid2 = array2D [
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

    printfn "Choose a Sudoku grid:\n1. Sample Puzzle 1\n2. Sample Puzzle 2"
    let choice = Console.ReadLine()

    let sudokuGrid =
        match choice with
        | "1" -> grid1
        | "2" -> grid2
        | _ -> 
            printfn "Invalid choice. Using Sample Puzzle 1 by default."
            grid1

    printfn "\nInitial Sudoku Grid:"
    printGrid sudokuGrid

    if solve sudokuGrid then
        printfn "Solved Sudoku Grid:"
        printGrid sudokuGrid
    else
        printfn "No solution exists."

    0 // Código de salida
