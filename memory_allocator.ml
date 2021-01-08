(*================================================================================ *)
(*==========  Doubly-linked list Queue using pointers and references  ============ *)
(*================================================================================ *)


module type Allocator = sig
  (* An abstract type for dynamic storage                                          *)
  type heap
  (* An abstract type for the pointer (address in the dynamic storage)             *)
  type ptr

  (* Create a new heap.                                                            *)
  val make_heap : unit -> heap

  (* Returns the "null" pointer. Noting can be assigned to it.                     *)
  val null : heap -> ptr
  val is_null : heap -> ptr -> bool

  (***                       Operations with pointers                            ***)
  (* All should throw exceptions for if the pointer is_null                        *)  

  (* Allocating a contiguous segment of dynamically-typed pointers in a heap.      *)
  (* Throws an specific "Out-Of-Memory" error if no allocation is possible.        *)
  val alloc : heap -> int -> ptr
  (* Frees the space in heap taken by the pointer.                                 *)
  val free : heap -> ptr -> int -> unit

  (* Dereferencing a pointer with an offset [0 .. n] obtainin a value it points to *)

  (* Dereference as an pointer, throw an exception if the target is not an pointer *)  
  val deref_as_ptr : heap -> ptr -> int -> ptr
  (* Dereference as an integer, throw an exception if the target is not an integer *)  
  val deref_as_int : heap -> ptr -> int -> int
  (* Dereference as an integer, throw an exception if the target is not an string  *)  
  val deref_as_string : heap -> ptr -> int -> string

  (* Assigning values to a pointer with an offset.                                 *)
  (* Should throw an "Out-Of-Memory" error if not possible to create a new value   *)
  (* The last argument is a value being assigned (of the corresponding type)       *)
  val assign_ptr : heap -> ptr -> int -> ptr -> unit
  val assign_int : heap -> ptr -> int -> int -> unit
  val assign_string : heap -> ptr -> int -> string -> unit

end;;

module DLLAllocator: Allocator = struct

type ptr = int * int option
type heap = {
    pointers: ptr array;
    integers: int array;
    strings: string array;
    trackptr: bool array
  }

(*type ptr = int * int option *)
         
let make_heap () = {
    pointers = Array.append  [| (0, None) |] (Array.make 1000 (1, Some 0));
    integers = Array.make 1000 0;
    strings = Array.make 1000 "" ;
    trackptr= Array.append [|false|] (Array.make 100 true)
  }

let null hp =
  hp.pointers.(0)

let is_null hp thispointer =
  if thispointer = (0, None)
  then true else false

let alloc hp n =
  let rec helper (track: bool array) n counter index a len =
    if a = n then Some index else
      if counter < len then(
        if track.(counter) = false
        then  helper track n (counter + 1) (counter + 1) 0 len
        else helper track n (counter + 1) index (a + 1) len)
      else  None
  in
  let res = helper hp.trackptr (n+1) 0 0 0 (Array.length(hp.trackptr)) in
  match res with
  | None -> raise (Failure "Unable to allocate")
  | Some x ->
     for i = x to (x + n) do
       hp.trackptr.(i) <- false
     done;
     hp.pointers.(x) <- (0, Some x);
     hp.pointers.(x)

(* i am editing the x - 1 in the above line. x*)

let free hp pointer n =
  let rec find_index array x i len=
    if i = len - 1 then None
    else (if array.(i) = x then Some i
          else find_index array x (i+1) len)
  in
  let res = find_index hp.pointers pointer 0 (Array.length(hp.pointers))
  in
  match res with
  | None -> raise(Failure "Pointer not in heap.")
  | Some x ->
     for i = x to (x + n) do
       hp.trackptr.(i) <- true
     done

let deref_as_ptr hp pointer o =
  let rec find_index array x i len=
    if i = len - 1 then None
    else (if array.(i) = x then Some i
          else find_index array x (i+1) len)
  in
  let res = find_index hp.pointers pointer 0 (Array.length(hp.pointers))
  in
  match res with
  | None -> raise(Failure "Pointer not in heap.")
  | Some p ->
     (match hp.pointers.(p + o + 1) with
      | (1, Some n) -> hp.pointers.(n)
      | _ -> raise(Failure "Does not refer to a pointer"))
  
let deref_as_int hp pointer o = 
  let rec find_index array x i len=
    if i = len - 1 then None
    else (if array.(i) = x then Some i
          else find_index array x (i+1) len)
  in
  let res = find_index hp.pointers pointer 0 (Array.length(hp.pointers))
  in
  match res with
  | None -> raise(Failure "Pointer not in heap.")
  | Some p ->
     (match hp.pointers.(p + o + 1) with
      | (2, Some n) -> hp.integers.(n)
      | _ -> raise(Failure "Does not refer to an integer"))
                                
let deref_as_string hp pointer o = 
  let rec find_index array x i len=
    if i = len - 1 then None
    else (if array.(i) = x then Some i
          else find_index array x (i+1) len)
  in
  let res = find_index hp.pointers pointer 0 (Array.length(hp.pointers))
  in
  match res with
  | None -> raise(Failure "Pointer not in heap.")
  | Some p ->
     (match hp.pointers.(p + o + 1) with
      | (3, Some n) -> hp.strings.(n)
      | _ -> raise(Failure "Does not refer to a string"))
  
let assign_ptr hp pointer o p2 =
  let rec find_index array x i len=
    if i = len - 1 then None
    else (if array.(i) = x then Some i
          else find_index array x (i+1) len)
  in
  let res = find_index hp.pointers pointer 0 (Array.length(hp.pointers)) in
  let res2 =  find_index hp.pointers p2 0 (Array.length(hp.pointers)) in
  match res, res2 with
  | Some p, Some b -> hp.pointers.(p + o+ 1) <- (1, Some b) 
  | _, _ -> raise(Failure "pointers not in heap")
     
                             
let assign_int hp pointer o i =
    let rec find_index array x i len=
    if i = len - 1 then None
    else (if array.(i) = x then Some i
          else find_index array x (i+1) len)
  in
  let res = find_index hp.pointers pointer 0 (Array.length(hp.pointers))
  in
  match res with
  | None -> raise(Failure "Pointer not in heap.")
  | Some p -> hp.pointers.(p + o + 1) <- (2, Some (p + o + 1));
              hp.integers.(p + o+ 1) <- i

  
let assign_string hp pointer o s=
  let rec find_index array x i len=
    if i = len - 1 then None
    else (if array.(i) = x then Some i
          else find_index array x (i+1) len)
  in
  let res = find_index hp.pointers pointer 0 (Array.length(hp.pointers))
  in
  match res with
  | None -> raise(Failure "Pointer not in heap.")
  | Some p -> hp.pointers.(p + o + 1) <- (3, Some (p + o + 1));
              hp.strings.(p + o + 1) <- s
end;;



(*

An incomplete double-linked list implemented via bare pointers,
   parameterised over the memory allocator interface A.

* N.B.: The dll_node is no longer type-safe, it is just a pointer, and
   you will have to implement all the functions in a memory-safe way.
   The only allowed crashes are "Out-Of-Memory" exceptions.

*)
module DoubleLinkedList(A: Allocator) = 
  struct
    open A
    type dll_node = ptr

    (* Example: creating a node with an integer and a string *)
    let mk_node heap i s = 
      let segment = alloc heap 4 in
      assign_int heap segment 0 i;
      assign_string heap segment 1 s;
      let z = null heap in
      assign_ptr heap segment 2 z;
      assign_ptr heap segment 3 z;
      segment
       
    let prev heap (n : dll_node) =  
      deref_as_ptr heap n 2


      
    let next heap (n : dll_node) = 
      deref_as_ptr heap n 3

      
    let int_value heap (n : dll_node) = 
      deref_as_int heap n 0

      
    let string_value heap (n : dll_node) =  
      deref_as_string heap n 1

      
          
    let insert_after heap (n1 : dll_node) (n2 : dll_node) = 
      let n = next heap n1 in
      (if is_null heap n then ()
       else assign_ptr heap n 2 n2);
      assign_ptr heap n2 3 n;
      assign_ptr heap n1 3 n2;
      assign_ptr heap n2 2 n1
      
    (* Prints the double-linked list starting from the node *)
    let print_node heap n =
      Printf.printf  "(%i, %s)" (int_value heap n) (string_value heap n)
      
    let print_from_node heap n =
        Printf.printf "[|";
      let rec helper heap' n' =
        if is_null heap' n' then Printf.printf "|]"
        else (print_node heap' n';
              if is_null heap' (next heap' n') = false then Printf.printf "; ";
              helper heap' (next heap' n'))
      in
      helper heap n
     
  (*  let remove heap n =
      free heap n 4;
      (if is_null heap (prev heap n)
      then ()
      else assign_ptr heap (next heap n) 2 (prev heap n));
      (if is_null heap (next heap n)
      then ()
      else assign_ptr heap (prev heap n) 3 (next heap n))  *)

      let remove heap n =
      free heap n 4;
      assign_ptr heap (next heap n) 2 (prev heap n);
      assign_ptr heap (prev heap n) 3 (next heap n)
      
  end 


(* testing the first two modules *)
module Intermediate = DoubleLinkedList(DLLAllocator);;
open Intermediate;;
open DLLAllocator;;
let a = make_heap ();;
let seg1 = mk_node a 89 "haan";;
let seg2 = mk_node a 90 "yeh";;
let seg3 = mk_node a 91 "yahan";;
null a = (next a seg3);;
print_from_node a seg1;;
insert_after a seg1 seg2;;
insert_after a seg2 seg3;;
print_node a seg1;;
print_node a seg2;;
print_node a seg3;;
print_from_node a seg1;;
print_from_node a seg2;;
print_from_node a seg3;;
is_null a (prev a seg2);;
remove a seg1;;
remove a seg2;;
print_from_node a seg3;;
let seg4 = mk_node a 78 "nahi";;
insert_after a seg3 seg4;;
print_from_node a seg3;;

  
(*

A familiar Queue interface:

*)

module type Queue =
sig
  type t
  val mk_queue : int -> t
  val is_empty : t -> bool
  val is_full :  t -> bool
  val enqueue :  t -> (int * string) -> unit
  val dequeue :  t -> (int * string) option
  val queue_to_list : t -> (int * string) list
end



(* A queue based on a double-linked list *)
 module HeapDLLQueue(A: Allocator) = struct
  module DLL = DoubleLinkedList(A)
  open A
  open DLL

  type t = {
    store : heap;
    head : dll_node;
    tail : dll_node;
    }
  let heap = make_heap ()
           
  let mk_queue _ =
    {store = heap;
     head = mk_node heap 1 "head";
     tail = mk_node heap 1 "tail"}

                 
  let is_empty q = 
     next q.store q.head = null q.store &&
       prev q.store q.head = null q.store

  let is_full q = false
                
                 
  let enqueue q e =
    let newnode = mk_node q.store (fst(e)) (snd(e)) in
    (if is_empty q
     then assign_ptr q.store q.head 3 newnode
     else insert_after q.store (deref_as_ptr q.store q.tail 3) newnode);
    assign_ptr q.store q.tail 3 newnode

     
  let dequeue q =
    if is_empty q then None
    else
      let n =  deref_as_ptr q.store q.head (3) in
      let nxt= next q.store n in
       assign_ptr q.store q.head (3) nxt;
      remove q.store n;
      let i = int_value q.store n in
      let s = string_value q.store n in
      Some(i, s)
    
  let queue_to_list q =
    let head =  deref_as_ptr q.store q.head (3) in
    let rec helper qq node list =
      if is_null qq node
      then list
      else
        (let i = int_value q.store node in
         let s = string_value q.store node in
         helper qq (next qq node) (List.rev_append [(i, s)] list) )
    in
    helper q.store head []
        
end;;

module MyQueue : Queue = HeapDLLQueue(DLLAllocator);;

open MyQueue;;
let a = mk_queue 8;;
dequeue a;;
enqueue a (8, "iou");;
enqueue a (9, "bas");;
queue_to_list a;;
dequeue a;;
queue_to_list a;;
enqueue a (78, "sahi");;
queue_to_list a;;
dequeue a;;
dequeue a;;
dequeue a;;
queue_to_list a;;
enqueue a (1, "yeh");;
queue_to_list a;;
