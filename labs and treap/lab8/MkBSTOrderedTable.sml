functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  (* Remove this line before submitting! *)
  exception NYI

  fun first (T : 'a table) : (key * 'a) option =
       case Tree.expose T of
        NONE => NONE
        |SOME {left,key,value,right} => if size left = 0 then SOME(key,value) else first left

  fun last (T : 'a table) : (key * 'a) option =
      case Tree.expose T of
        NONE => NONE
        |SOME {left,key,value,right} => if size right = 0 then SOME(key,value) else last right
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
      let
        val (L,_,_) = Tree.splitAt(T,k)
      in
        last L
      end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
      let
        val (_,_,R) = Tree.splitAt(T,k)
      in
        first R
      end

  fun join (L : 'a table, R : 'a table) : 'a table =
      Tree.join (L,R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
      Tree.splitAt (T,k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
      let
        val (L1,x,R1) = Tree.splitAt(T,low)
        val T1 = case x of
                 NONE => R1
                 |SOME v => Tree.join(singleton(low,v),R1)
        val (L2,y,R2) = Tree.splitAt(T1,high)
        val T2 = case y of
                 NONE => L2
                 |SOME v => Tree.join(L2,singleton(high,v))
      in
        T2
      end
						       

end
