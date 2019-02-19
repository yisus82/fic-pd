PROGRAMACION DECLARATIVA

PRACTICA 4

FUNCIONES QUE MANEJAN LISTAS:

--Funcion length: Devuelve la longitud de la lista dada

let rec length = function [] -> 0 
	|(h::t) -> 1 + length(t);;

val length : 'a list -> int = <fun>

--Funcion hd: Devuelve el primer elemento de la lista dada (cabeza)

let rec hd = function [] -> raise (Failure "hd")
	|(h::_) -> h;;

val hd : 'a list -> 'a = <fun>
 
--Funcion tl: Devuelve la cola de la lista (todos los elementos menos el primero)

let rec tl = function [] -> raise (Failure "tl")
	|(_::t) -> t;;

val tl 'a list -> 'a list = <fun>

--Funcion nth: Devuelve el elemento de la lista en la posicion dada

let nth l m = let rec aux (x,i) = match x with
	[] -> raise
	|h::t -> if i=m then h
		else aux(t, i+1)
	in aux (l, 1);;

val nth : (exn -> 'a) list -> int -> exn -> 'a = fun

--Funcion rev: Devuelve la lista invertida

let rev l = let rec aux (l1, l2) = match l1 with 
	[] -> l2
	|h::t -> aux (t, h::l2)
	in aux (l,[]);;

val rev : 'a list -> 'a list = <fun>

--Funcion append: Devuelve la concatenacion de dos listas.

let rec append l1 l2 = match l1 with
	[] -> if l2=[] then []
		else append l2 []
	|h::t -> h:: append t l2;;

val append : 'a list -> 'a list -> 'a list = <fun>

--Funcion for_all p [a1; a2; ... ; a3]: compruba si tosod los elementos de la lista satisfacen 
	el predicado p

let rec for_all p = function l -> if l = [] then true
	else p (hd l) & for_all p (tl l);;

val for_all : ('a -> bool) -> 'a list -> bool = <fun>

--Funcion exists p [a1; a2; ...; a3]: devuelve true si alguno de los elementos de la lista 
	cumple el predicado p

let res exists p = function l -> if l = [] then false
	else p (hd l) or exists p (tl l);;

val exists : ('a -> bool) -> 'a list -> bool

--Funcion mem a l: es true si y solo si a es igual a un elemento de l

let rec mem a = function l -> if l=[] then false
	else if a = hd l then true else mem a (tl l);;

val mem : 'a -> 'a list -> bool = <fun>

--Funcion find p l: devuelve el primer elemento de la lista l que satisface el predicado p

let rec find p = function l -> if l=[] then raise(Not_found)
	else if p (hd l) then true 
		else find p (tl l);;

val find : ('a -> bool) -> 'a list -> bool = <fun>

 

FUNCION DE FIBONACCI

RECURSION NO TERMINAL

let rec fib n =
	if n = 0 then 0
		else if n = 1 then 1
			else fib(n-1) + fib(n-2);;

RECURSION TERMINAL

let fib n=
    let rec proceso(i,ult,pen)=
	if i=n then ult
	else proceso(i+1,ult+pen,ult)
	in proceso(1,1,0);;

El último entero representable es el 44. 
#fib 44;;
- : int = 701408733


