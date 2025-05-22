namespace Busqueda
open Capitulo3
module DFS =
    open Pila

    let estrategia<'s,'a> =
        {siguiente   = pop
         agregar     = push
         inicializar = push empty
         } : estrategia<'s,'a,pila<nodo<'s,'a>>>

    let id n = n.estado