namespace Sokoban

module ListUtils =    
    let replaceWith matchFun newItem list = 
        let rec replaceRec acc = 
            function 
            | head :: tail when (matchFun head) -> tail |> List.append ((newItem :: acc) |> List.rev) 
            | head :: tail -> replaceRec (head :: acc) tail
            | _ -> acc |> List.rev
        replaceRec [] list

