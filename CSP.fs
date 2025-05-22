namespace CSP
open Capitulo3
open Busqueda

module CSP =
    open Capitulo3
    open Utils

    type estado<'variable,'valor when 'variable: comparison and
                                      'valor   : comparison>
        = Map<'variable, 'valor list>

    type restriccion<'variable,'valor when 'variable: comparison and
                                           'valor   : comparison>
        =  Binaria of ('variable * 'variable) * (estado<'variable,'valor> -> bool)
         | NAria of ('variable list) * (estado<'variable,'valor> -> bool)

    type csp<'variable,'valor when 'variable: comparison and
                                   'valor   : comparison> =
        {variables     : 'variable list
         dominios      : 'valor list list
         restricciones : restriccion<'variable,'valor> list
        }

    let eval r =
        match r with
            | Binaria (_, f) -> f
            | NAria (_, f) -> f

    let destBinaria = function
        | Binaria (r, f) -> Some (r, f)
        | _ -> None

    let inicio (csp : csp<'variable,'valor>) =
        csp.dominios 
        |> List.map2 (fun v dom -> (v, dom)) csp.variables
        |> List.fold (fun m (v,dom) -> Map.add v dom m) Map.empty

    let meta (estado : estado<'variable, 'valor>) =
        Map.forall (fun v dom -> List.length dom = 1) estado

    let degree var csp =
        List.fold (fun c r -> match r with
                                | Binaria ((v,v'), _) ->
                                    if var = v || var = v'
                                    then c + 1
                                    else c
                                | NAria (vs, _) -> 
                                    if List.exists (fun v -> v = var) vs
                                    then c + 1
                                    else c) 0 csp.restricciones

    let ac3 csp estado =
        let incidencias v =
            csp.restricciones
            |> List.choose destBinaria
            |> List.choose (fun ((p, q), f) -> 
                    if v = p
                    then Some (Binaria ((p, q), f), (q, v))
                    elif v = q
                    then Some (Binaria ((p, q), f), (p, v))
                    else None)
        let remover estado (restriccion, (v,v')) =
            estado |> Map.find v
                   |> List.fold (fun (modificado, dom) x ->
                            let estado' = Map.add v [x] estado
                            if List.exists (fun y -> let estado'' = Map.add v' [y] estado'
                                                     eval restriccion estado'')
                                           (Map.find v' estado)
                            then (modificado, x::dom)
                            else (true, dom)) (false, [])
        let rec ac3_aux estado restricciones =
            match restricciones with
                | (r, (v,v')) :: restricciones ->
                    let (modificado, dom) = remover estado (r, (v,v'))
                    if modificado
                    then let estado = Map.add v dom estado
                         let arcos = incidencias v
                         ac3_aux estado (arcos @ restricciones)
                    else ac3_aux estado restricciones
                | [] -> estado
        csp.restricciones
            |> List.collect (fun r -> match r with
                                        | Binaria ((p,q), _) ->
                                            [(r, (p,q)); (r, (q,p))]
                                        | NAria _ -> [])
            |> ac3_aux estado

    let sucesores (csp : csp<'variable,'valor>) (estado : estado<'variable, 'valor>) =
        Map.toList estado
        |> List.filter (fun (_, dom) -> List.length dom > 1)
        |> (fun l -> match l with
                     | _ :: _ -> 
                         l |> List.minBy (fun (var, dom) -> (List.length dom, -degree var csp))
                           |> (fun (var, dom) -> 
                               List.choose (fun v -> 
                                    let s = estado |> Map.add var [v]
                                                   |> ac3 csp
                                    if not (Map.exists (fun _ vs -> List.isEmpty vs) s) &&
                                       List.forall (fun r -> eval r s) csp.restricciones
                                    then Some ((), s)
                                    else None) dom)
                     | _ -> [])

    let public backtracking (csp : csp<'variable,'valor>) =
        let problema = 
            {inicio = inicio csp
             meta = meta
             sucesores = sucesores csp
             costo = fun _ _ _ -> 1.0
            } : problema<estado<'variable,'valor>, unit>
        busquedaGrafo DFS.id DFS.estrategia problema

    let minimosConflictos (csp : csp<'variable,'valor>) max_iteraciones =
        let rnd = System.Random()
        let n = List.length csp.variables
        let estado = List.map2 (fun var dom -> (var, [List.item (rnd.Next(n)) dom]))
                               csp.variables
                               csp.dominios
                     |> Map.ofList
        let vars_dom = List.map2 (fun v dom -> (v, dom)) csp.variables csp.dominios
                       |> Map.ofList
        let h estado = List.sumBy (fun r -> if eval r estado then 0 else 1)
                                  csp.restricciones
        let rec loop i estado =
            if i < max_iteraciones
            then if List.forall (fun r -> eval r estado) csp.restricciones
                 then Some estado
                 else let var = List.filter (fun r -> not (eval r estado)) csp.restricciones
                                |> List.choose destBinaria
                                |> List.collect ((fun (v1,v2) -> [v1;v2]) << fst)
                                |> List.countBy id
                                |> (Util.seleccionarInt rnd << List.toArray)
                      let dom = vars_dom.[ var ]
                      let valor = List.minBy (fun v -> h (Map.add var [v] estado)) dom
                      loop (i + 1) (Map.add var [valor] estado)
            else None
        loop 0 estado

    let minimosConflictosReinicio (csp : csp<'variable,'valor>) max_iteraciones =
        let rec loop i =
            printfn "%i" i
            match minimosConflictos csp max_iteraciones with
            | Some estado -> estado
            | None -> loop (i + 1)
        loop 0