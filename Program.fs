open System
open System.Collections.Generic

// Imprime el tablero de Sudoku
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

// Obtiene la lista de vecinos (restricciones) para una celda
let vecinos (fila, col) =
    let filaV = [ for c in 0 .. 8 -> (fila, c) ]
    let colV = [ for r in 0 .. 8 -> (r, col) ]
    let startR = fila / 3 * 3
    let startC = col / 3 * 3
    let subV = [ for r in startR .. startR + 2 do for c in startC .. startC + 2 -> (r, c) ]
    Set.ofList (filaV @ colV @ subV) |> Set.remove (fila, col)

// Inicializa dominios para todas las celdas
let initDominios (tabla: int[,]) =
    let dominios = Dictionary<(int * int), Set<int>>()
    for fila in 0 .. 8 do
        for col in 0 .. 8 do
            if tabla.[fila, col] = 0 then
                dominios.[(fila, col)] <- Set.ofList [1..9]
    dominios

// Asigna dominios válidos reducidos según valores vecinos
let reducirDominios (tabla: int[,]) (dominios: Dictionary<_,_>) =
    for kvp in dominios do
        let (fila, col) = kvp.Key
        let posibles = kvp.Value
        let usados =
            vecinos (fila, col)
            |> Seq.choose (fun (f, c) -> if tabla.[f, c] <> 0 then Some tabla.[f, c] else None)
            |> Set.ofSeq
        dominios.[(fila, col)] <- Set.difference posibles usados

// Obtiene la celda con menor dominio (heurística MRV)
let seleccionarVariable (dominios: Dictionary<_,_>) =
    dominios
    |> Seq.minBy (fun kvp -> Set.count kvp.Value)
    |> fun kvp -> kvp.Key, kvp.Value


// Copia el diccionario de dominios
let copiarDominios (dom: Dictionary<(int * int), Set<int>>) =
    let nuevo = Dictionary<(int * int), Set<int>>()
    for kvp in dom do
        nuevo.Add(kvp.Key, kvp.Value)
    nuevo

// Algoritmo CSP Backtracking con Forward Checking
let rec resolverCSP (tabla: int[,]) (dominios: Dictionary<_,_>) =
    if dominios.Count = 0 then true
    else
        let (fila, col), valores = seleccionarVariable dominios
        valores
        |> Seq.exists (fun valor ->
            let copiaTabla = tabla.Clone() :?> int[,]
            copiaTabla.[fila, col] <- valor

            let nuevosDominios = copiarDominios dominios
            nuevosDominios.Remove((fila, col)) |> ignore

            let mutable esValido = true

            for (f, c) in vecinos (fila, col) do
                if nuevosDominios.ContainsKey((f, c)) then
                    nuevosDominios.[(f, c)] <- Set.remove valor nuevosDominios.[(f, c)]
                    if nuevosDominios.[(f, c)].IsEmpty then
                        esValido <- false

            if esValido && resolverCSP copiaTabla nuevosDominios then
                // Copia solución
                for f in 0 .. 8 do
                    for c in 0 .. 8 do
                        tabla.[f, c] <- copiaTabla.[f, c]
                true
            else false
        )

// Función principal
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

    let dominios = initDominios tabla
    reducirDominios tabla dominios

    if resolverCSP tabla dominios then
        printfn "Sudoku resuelto:"
        printTabla tabla
    else
        printfn "No existe solución :("

    0
