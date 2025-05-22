namespace Busqueda
open Capitulo3

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


module Capitulo4 =
    open Capitulo3

    let ascensionColina problema h =
        let nodo_inicio = 
            {estado = problema.inicio
             padre = None
             profundidad = 0
             accion = None
             costo_ruta = 0.0}
        let estadistica = estadistica_inicial
        let rec loop (std, actual) =
            let sucs = nodos_hijos problema actual
            let std = actualizar_estadistica std sucs
            let vecino = List.maxBy h sucs
            if h vecino <= h actual
            then if problema.meta actual.estado
                 then std, Some actual
                 else std, None
            else loop (std, vecino)
        loop (estadistica, nodo_inicio)

    // temperatura 1.0 0.045 100.0
    let temperatura k lam limit t =
        if t < limit
        then k * System.Math.Exp((-1.0) * lam * t)
        else 0.0

    let recocidoSimulado schedule problema h =
        let rnd = System.Random()
        let nodo_inicio = 
            {estado = problema.inicio
             padre = None
             profundidad = 0
             accion = None
             costo_ruta = 0.0}
        let estadistica = estadistica_inicial
        let rec loop t (std, actual) =
            let T = schedule t
            if T = 0.0
            then if problema.meta actual.estado
                 then std, Some actual
                 else std, None
            else let sucs = nodos_hijos problema actual
                 let std = actualizar_estadistica std sucs
                 let siguiente = sucs.[rnd.Next(sucs.Length)]
                 let deltaE = h siguiente - h actual
                 if deltaE > 0.0 ||
                    rnd.NextDouble() <= System.Math.Exp(deltaE / T)
                 then loop (t+1.0) (std, siguiente)
                 else loop (t+1.0) (std, actual)
        loop 0.0 (estadistica, nodo_inicio)

    let reinicio algoritmo problema h =
        let rec loop std =
            match algoritmo (problema ()) h with
            | (std', Some n) -> (sumar_estadistica std std', Some n)
            | (std', None) -> let std = sumar_estadistica std std'
                              loop std
        loop estadistica_inicial
