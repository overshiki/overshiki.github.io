


type _ typ = 
| Int : int typ
| String : string typ
| Pair : 'a typ * 'b typ -> ('a * 'b) typ


let rec to_string : type t. t typ -> t -> string = 
  fun t x -> 
  match t with 
  | Int -> Int.to_string x
  | String -> Printf.sprintf "%S" x
  | Pair (t1, t2) -> 
      let (x1, x2) = x in
      Printf.sprintf "(%s, %s)" (to_string t1 x1) (to_string t2 x2)