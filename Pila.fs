namespace Busqueda

type 'a pila = 'a list

module Pila =
    let empty = []

    let push pila x = x :: pila

    let pop = function
        | x :: pila -> Some (x, pila)
        | [] -> None
