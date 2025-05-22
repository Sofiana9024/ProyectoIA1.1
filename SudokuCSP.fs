module SudokuCSP

open CSP
open SudokuUtils

let construirSudokuCSP (tabla: int[,]) =
    let todasLasPosiciones =
        [ for fila in 0 .. 8 do for col in 0 .. 8 -> (fila, col) ]

    let dominioPorDefecto = [1..9]

    let dominiosMap =
        todasLasPosiciones
        |> List.map (fun (fila, col) ->
            let valor = tabla.[fila, col]
            if valor = 0 then ((fila, col), dominioPorDefecto)
            else ((fila, col), [valor])
        )
        |> Map.ofList

    let dominiosLista : int list list =
        todasLasPosiciones
        |> List.map (fun pos ->
            match Map.tryFind pos dominiosMap with
            | Some valores -> valores
            | None -> []
        )

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