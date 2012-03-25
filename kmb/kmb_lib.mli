type 'a result = Parsed of 'a * Kmb_input.input | Failed
val return : 'a -> Kmb_input.input -> 'a result
val transform : ('a -> 'b result) -> ('b -> 'c) -> 'a -> 'c result
val predicate_not :
  (Kmb_input.input -> 'a result) -> Kmb_input.input -> unit result
val predicate_and :
  (Kmb_input.input -> 'a result) -> Kmb_input.input -> 'a result
val peg_stub : Kmb_input.input -> unit result
val opt : (Kmb_input.input -> unit result) -> Kmb_input.input -> unit result
val opt_accu :
  (Kmb_input.input -> 'a result) -> Kmb_input.input -> 'a option result
val get_lexeme :
  (Kmb_input.input -> unit result) ->
  Kmb_input.input -> Kmb_input.lexeme result
val test_any : Kmb_input.input -> unit result
val test_char : int -> Kmb_input.input -> unit result
val match_pattern : int list -> Kmb_input.input -> unit result
val test_class : (int -> bool) -> Kmb_input.input -> unit result
val get_pattern :
  (Kmb_input.input -> unit result) -> Kmb_input.input -> int list result
val seq_l :
  ('a -> 'b result) -> (Kmb_input.input -> unit result) -> 'a -> 'b result
val seq_r :
  ('a -> unit result) -> (Kmb_input.input -> 'b result) -> 'a -> 'b result
val seq_n :
  ('a -> unit result) ->
  (Kmb_input.input -> unit result) -> 'a -> unit result
val seq_b :
  ('a -> 'b result) ->
  (Kmb_input.input -> 'c result) -> 'a -> ('b * 'c) result
val alt : ('a -> 'b result) -> ('a -> 'b result) -> 'a -> 'b result
val star : (Kmb_input.input -> unit result) -> Kmb_input.input -> unit result
val star_accu :
  (Kmb_input.input -> 'a result) -> Kmb_input.input -> 'a list result
val plus : (Kmb_input.input -> unit result) -> Kmb_input.input -> unit result
val plus_accu :
  (Kmb_input.input -> 'a result) -> Kmb_input.input -> 'a list result
