functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    if length buildings <> 0 then
      case showt buildings of
        EMPTY => singleton((0,0))
        | ELT (l,h,r) => % [(l,h),(r,0)]
        | NODE (ls,rs) =>
        let
          fun combine p1 p2 =
            let
              fun f1 (x,y) = (x,y,1)
              fun f2 (x,y) = (x,y,2)
              fun cmp ((x1,_,_),(x2,_,_)) =
               case x1 > x2 of
                true => GREATER
                | false => LESS
              val p1' = map f1 p1
              val p2' = map f2 p2
              val p_beta = merge cmp p1' p2'
              fun f_scan1 ((l1,h1,m1),(l2,h2,m2)) =
                if m1=2 andalso m2=2 then (l2,0,2)
                else if m1=1 andalso m2=2 then (l2,h1,1)
                else if m1=2 andalso m2=1 then (l2,h2,1)
                else (*r1=r2=1*) (l2,h2,1)
              fun f_scan2 ((l1,h1,m1),(l2,h2,m2)) =
                if m1=1 andalso m2=1 then (l2,0,1)
                else if m1=2 andalso m2=1 then (l2,h1,2)
                else if m1=1 andalso m2=2 then (l2,h2,2)
                else (*r1=r2=2*) (l2,h2,2)
              val p1'' = scani f_scan1 (0,0,2) p_beta
              val p2'' = scani f_scan2 (0,0,1) p_beta
              fun f_zip ((l1,h1,_),(l2,h2,_)) = 
                case h1 > h2 of
                true => (l1,h1)
                | false => (l2,h2)
              val rep_rst = map f_zip (zip p1'' p2'')                 
            in
              filterIdx (fn (i,(_,y)) => i=0 orelse (#2 (nth rep_rst (i-1)) <> y)) rep_rst
            end
          val (points1, points2) = par(fn () => skyline ls, fn () => skyline rs)
        in
          combine points1 points2
        end
    else empty()
end