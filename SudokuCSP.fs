module SudokuCSP
open Capitulo3
open Busqueda
open CSP

type Pos = int * int  // Una posición en el tablero: (fila, columna)

// Dominio para Sudoku 9x9
let valores = [1 .. 9]

// Genera todas las variables del tablero
let todasLasPosiciones : Pos list =
    [ for fila in 0 .. 8 do
        for col in 0 .. 8 -> (fila, col) ]

// Función para obtener las restricciones binarias de Sudoku
let restriccionesSudoku : (Pos * Pos -> bool) list =
    let distinta (x: int) (y: int) = x <> y

    let mismaFila (x1, y1) (x2, y2) = x1 = x2 && y1 <> y2
    let mismaCol (x1, y1) (x2, y2) = y1 = y2 && x1 <> x2
    let mismoCuadro (x1, y1) (x2, y2) =
        (x1 / 3 = x2 / 3) && (y1 / 3 = y2 / 3) && (x1, y1) <> (x2, y2)

    let vecinos (v1: Pos) (v2: Pos) =
        (mismaFila v1 v2 || mismaCol v1 v2 || mismoCuadro v1 v2)

    // Devuelve restricciones binarias: si son vecinos, deben ser distintos
    [ fun v1 v2 -> not (vecinos v1 v2) || distinta ]
    |> List.map (fun cond -> fun v1 v2 -> fun val1 val2 -> cond v1 v2 val1 val2)

// Construcción del CSP a partir de un tablero inicial (lista de listas)
let construirSudokuCSP (tablero: int list list) : CSP<Pos, int> =
    let dominio pos =
        match tablero.[fst pos].[snd pos] with
        | 0 -> valores // Casilla vacía
        | fijo -> [fijo] // Casilla prellenada

    let dominios = [ for v in todasLasPosiciones -> (v, dominio v) ]

    // Generar todas las restricciones binarias para cada par de variables
    let restricciones =
        [ for v1 in todasLasPosiciones do
            for v2 in todasLasPosiciones do
                if v1 <> v2 && (
                    let (x1, y1) = v1
                    let (x2, y2) = v2
                    (x1 = x2 || y1 = y2 || (x1 / 3 = x2 / 3 && y1 / 3 = y2 / 3))
                )
                then
                    yield (v1, v2, fun val1 val2 -> val1 <> val2)
        ]

    let soloDominios = List.map snd dominios  // tipo: int list list

    let sudokuCSP = {
        variables = todasLasPosiciones
        dominios = soloDominios
        restricciones = restricciones
    }

    

