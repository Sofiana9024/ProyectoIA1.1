namespace Utils

module Util =
    open System

    // Selecciona un elemento de la lista con una 
    // probabilidad directamente proporcional a la 
    // segunda componente del par.
    let seleccionarDouble (rnd : Random) (L : ('a * double) []) =
        let choosen = rnd.NextDouble() * Array.sumBy (fun (_, w) -> w) L
        let rec fld indx (L : ('a * double) []) i =
            let (x, w) = L.[indx]
            let i = i + w
            if choosen < i then x
            else fld (indx + 1) L i
        fld 0 L 0.0

    let seleccionarInt (rnd : Random) (L : ('a * int) []) =
        let choosen = rnd.Next(Array.sumBy (fun (_, w) -> w) L)
        let rec fld indx (L : ('a * int) []) i =
            let (x, w) = L.[indx]
            let i = i + w
            if choosen < i then x
            else fld (indx + 1) L i
        fld 0 L 0