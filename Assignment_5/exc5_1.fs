let rec mergeList (xs,ys) =
    match (xs, ys) with
    | (x::xr, y::yr) -> if x<y then x :: mergeList (xr, ys) else y :: mergeList (xs, yr)
    | ([], ys) -> ys
    | (xs, []) -> xs 
    | ([],[]) -> []
