namespace Capitulo3
open Busqueda

type nodo<'s,'a> = 
    {estado      : 's
     padre       : nodo<'s,'a> option
     profundidad : int
     accion      : 'a option
     costo_ruta  : float
    }
    
type problema<'s,'a> =
    {inicio    : 's
     meta      : 's -> bool
     sucesores : 's -> ('a * 's) list
     costo     : 's -> 'a -> 's -> float
    }
    
type estrategia<'s,'a,'d> =
    {siguiente   : 'd -> (nodo<'s,'a> * 'd) option
     agregar     : 'd -> nodo<'s,'a> -> 'd
     inicializar : nodo<'s,'a> -> 'd
    }

type estadistica =
    {nodos_generados : uint64
     nodos_procesados: uint64
    }

type datosBidireccional<'s,'a,'b,'d when 's : comparison> =
    {estados : Map<'s,nodo<'s,'a>>
     procesados : 'b
     estadistica : estadistica
     bolsa : 'd
    }

module Capitulo3 =
    let estadistica_inicial =
        {nodos_generados = 1UL
         nodos_procesados = 0UL}

    let actualizar_estadistica estadistica sucesores =
        {nodos_generados = estadistica.nodos_generados + uint64 (List.length sucesores)
         nodos_procesados = estadistica.nodos_procesados + 1UL}

    let incremente_estadistica estadistica =
        {nodos_generados = estadistica.nodos_generados + 1UL
         nodos_procesados = estadistica.nodos_procesados + 1UL}

    let sumar_estadistica std std' =
        {
            nodos_generados = std.nodos_generados + std'.nodos_generados
            nodos_procesados = std.nodos_procesados + std'.nodos_procesados
        }

    let nodos_hijos problema padre =
        padre.estado |> problema.sucesores
                     |> List.map (fun (accion, estado) ->
                                    {estado = estado
                                     padre = Some padre
                                     profundidad = padre.profundidad + 1
                                     accion = Some accion
                                     costo_ruta = padre.costo_ruta + problema.costo padre.estado accion estado
                                    })

    let busquedaArbol estrategia problema =
        let nodo_inicio = 
            {estado = problema.inicio
             padre = None
             profundidad = 0
             accion = None
             costo_ruta = 0.0}
        let bolsa = estrategia.inicializar nodo_inicio
        let std = estadistica_inicial
        let rec loop (std, bolsa) =
            match estrategia.siguiente bolsa with
                | Some (s, bolsa) ->
                            if problema.meta s.estado
                            then (std, Some s)
                            else s |> nodos_hijos problema
                                   |> (fun ss -> (actualizar_estadistica std ss, 
                                                  List.fold estrategia.agregar bolsa ss))
                                   |> loop
                | None -> (std, None)
        loop (std, bolsa)

    let busquedaGrafo id estrategia problema =
        let nodo_inicio = 
            {estado = problema.inicio
             padre = None
             profundidad = 0
             accion = None
             costo_ruta = 0.0}
        let bolsa = estrategia.inicializar nodo_inicio
        let std = estadistica_inicial
        let rec loop procesados (std, bolsa) =
            match estrategia.siguiente bolsa with
                | Some (s, bolsa) -> 
                    if problema.meta s.estado
                    then (std, Some s)
                    else if Set.contains (id s) procesados
                         then loop procesados
                                   (actualizar_estadistica std [],
                                    bolsa)
                         else s |> nodos_hijos problema
                                |> (fun ss -> (actualizar_estadistica std ss, 
                                               List.fold estrategia.agregar bolsa ss))
                                |> loop (Set.add (id s) procesados)
                | None -> (std, None)
        loop Set.empty (std, bolsa)

    let busquedaBidireccional meta invierte id estrategia problema =
        let nodo_inicio = 
            {estado = problema.inicio
             padre = None
             profundidad = 0
             accion = None
             costo_ruta = 0.0}
        let nodo_meta = 
            {estado = meta
             padre = None
             profundidad = 0
             accion = None
             costo_ruta = 0.0}
        let bolsa = estrategia.inicializar nodo_inicio
        let bolsa' = estrategia.inicializar nodo_meta
        let std = estadistica_inicial
        let rec transforma s s' =
            match s'.padre with
            | Some n' -> let n = {estado = n'.estado
                                  padre  = Some s
                                  profundidad = s.profundidad + 1
                                  accion      = (Some << invierte << Option.get) s'.accion
                                  costo_ruta  = s.costo_ruta + s'.costo_ruta - n'.costo_ruta}
                         transforma n n'
            | None -> s
        let rec loop foo datos datos' =
            match estrategia.siguiente datos.bolsa with
                | Some (s, bolsa) ->
                    if foo && problema.meta s.estado
                    then (datos.estadistica, Some s)
                    elif Map.containsKey s.estado datos'.estados
                    then let estadistica = {
                             nodos_generados = datos.estadistica.nodos_generados + 
                                               datos'.estadistica.nodos_generados
                             nodos_procesados = datos.estadistica.nodos_procesados + 
                                                datos'.estadistica.nodos_procesados
                             }
                         if foo
                         then (estadistica, Some (transforma s (datos'.estados.[ s.estado ])))
                         else (estadistica, Some (transforma (datos'.estados.[ s.estado ]) s))
                    else if Set.contains (id s) datos.procesados
                         then loop (not foo) datos' {datos with estadistica = actualizar_estadistica datos.estadistica []
                                                                bolsa = bolsa}
                         else s |> nodos_hijos problema
                                |> (fun ss -> (actualizar_estadistica datos.estadistica ss,
                                               List.fold estrategia.agregar bolsa ss))
                                |> (fun (sts, bolsa) -> 
                                        loop (not foo)
                                             datos' 
                                             {procesados = Set.add (id s) datos.procesados
                                              estados = Map.add s.estado s datos.estados
                                              estadistica = sts
                                              bolsa = bolsa})
                | None -> (std, None)
        loop true
             {procesados = Set.empty
              estados = Map.empty
              estadistica = std
              bolsa = bolsa}
             {procesados = Set.empty
              estados = Map.empty
              estadistica = std
              bolsa = bolsa'}

    let rec acciones nodo =
        match nodo.padre with
            | Some padre -> acciones padre @ [Option.get nodo.accion]
            | None       -> []

    let effective_branching_factor x0 (N : uint64) d =
        let N = float N
        let d = float d
        let rec newton x =
            let f = x ** (d+1.0) - x * (N+1.0) + N
            let f' = (d + 1.0) * x ** d - (N + 1.0)
            let x' = x - f / f'
            if System.Math.Abs (x' - x) < 0.0000001
            then x'
            else newton x'
        newton x0