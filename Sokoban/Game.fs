namespace Sokoban

open Sokoban.ListUtils

module Game =

    type Move = 
    | Up
    | Left
    | Right
    | Down

    type X = X of int

    type Y = Y of int

    type Coord = Coord of X * Y
        with 
            static member move (m: Move) (Coord((X x), (Y y))) = 
                match m with 
                | Up -> Coord((X x), Y(y + 1))
                | Left -> Coord(X(x - 1), Y(y))
                | Down -> Coord(X(x), Y(y - 1))
                | Right -> Coord(X(x + 1), Y(y))
            static member areEqual (Coord((X x1), (Y y1))) (Coord((X x2), (Y y2))) = 
                x1 = x2 && y1 = y2

    type Element = 
    | Wall 
    | Box 
    | StockedBox 
    | Stock 
    | Human 
    | HumanOnStock
    | Space 
        with
            static member toString = 
                function
                | Wall -> "#"
                | Box -> "$"
                | StockedBox -> "*"
                | Stock -> "."
                | Human -> "@"
                | HumanOnStock -> "+"
                | Space -> " "

    type Position = Position of Element*Coord
        with
            static member toString (p: Position) = 
                match p with 
                | Position(e, _) -> Element.toString e

            static member parse j i =
                function
                | '@' -> Position(Human, Coord(X(i), Y(j)))
                | '+' -> Position(HumanOnStock, Coord(X(i), Y(j)))
                | '#' -> Position(Wall, Coord(X(i), Y(j)))
                | '$' -> Position(Box, Coord(X(i), Y(j)))
                | '*' -> Position(StockedBox, Coord(X(i), Y(j)))
                | '.' -> Position(Stock, Coord(X(i), Y(j)))
                | _ -> Position(Space, Coord(X(i), Y(j)))

            static member hasCoord (c: Coord) (p: Position) = 
                match c, p with
                | Coord(x1, y1), Position(_, Coord(x2, y2)) when x1 = x2 && y1 = y2 -> true
                | _ -> false

            static member getCoord (p: Position) = 
                match p with 
                | Position(_, coord) -> coord

            static member tryGetCoord (pos: Position option) = 
                match pos with 
                | Some(p) -> Some(Position.getCoord p)
                | None -> None

            static member haveSameCoord (p1: Position) (p2: Position) =
                Position.getCoord p1 |> Coord.areEqual (p2 |> Position.getCoord)

    type Line = Line of Position list
        with
            static member toString (Line positions) = 
                positions |> List.fold(fun acc p -> acc + (Position.toString p)) ""

            static member parse (i: int) (source: string) = 
                source 
                |> Seq.mapi(Position.parse i) 
                |> Seq.toList 
                |> Line 

            static member replace newPosition (Line positions) = 
                positions 
                |> replaceWith (Position.hasCoord (Position.getCoord newPosition)) newPosition 
                |> Line

            static member tryFindPosition coord = List.tryFind(Position.hasCoord coord)

            static member tryFindHuman (Line positions) = 
                let rec tryFindHumanRec positions =
                    match positions with
                    | head :: tail -> 
                        match head with
                        | Position(Human, _) | Position(HumanOnStock, _) -> Some(head)
                        | _ -> tryFindHumanRec(tail)
                    | _ -> None
                tryFindHumanRec positions

    type Game = Game of Line list 
        with
            static member toString (Game lines) = 
                lines 
                |> List.rev
                |> List.fold(fun acc l -> acc + "\r\n" + (Line.toString l)) ""

            static member print game = 
                game 
                |> Game.toString 
                |> printfn "%s"
            
            static member parse sources = 
                sources 
                |> List.mapi(Line.parse) 
                |> Game

            static member readFile fileName = 
                System.IO.File.ReadAllLines(fileName) 
                |> Seq.toList 
                |> List.rev
                |> Game.parse

            static member replace newPosition (Game lines) = 
                lines 
                |> List.map(Line.replace newPosition) 
                |> Game

            static member tryFindHuman (Game lines) = 
                let rec tryFindHumanInLines (lines: Line list) = 
                    match lines with 
                    | head :: tail -> 
                        match Line.tryFindHuman head with
                        | Some(human) -> Some(human)
                        | _ -> tryFindHumanInLines(tail)
                    | _ -> None
                tryFindHumanInLines lines

            static member tryFindPosition coord (Game lines)= 
                match lines with
                | Line(positions) :: tail -> 
                    match Line.tryFindPosition coord positions  with
                    | Some(p) -> Some(p)
                    | _  -> Game.tryFindPosition coord (Game tail)
                | _ -> None

            static member tryFindNextPosition move game =
                function
                | None -> None
                | Some(source) -> 
                    let srcCoord = Position.getCoord source
                    let targetCoord = Coord.move move srcCoord
                    game |> Game.tryFindPosition targetCoord  

            static member tryMovePosition source destination game = 
                let movePositions (source: Position) (destination: Position) = 
                    match source, destination with
                    | Position(Human, srcCoord), Position(Space, destCoord) -> Position(Space, srcCoord), Position(Human, destCoord)
                    | Position(Human, srcCoord), Position(Stock, destCoord) -> Position(Space, srcCoord), Position(HumanOnStock, destCoord)
                    | Position(HumanOnStock, srcCoord), Position(Space, destCoord) -> Position(Stock, srcCoord), Position(Human, destCoord)
                    | Position(HumanOnStock, srcCoord), Position(Stock, destCoord) -> Position(Stock, srcCoord), Position(HumanOnStock, destCoord)
                    | Position(Box, srcCoord), Position(Space, destCoord) -> Position(Space, srcCoord), Position(Box, destCoord)
                    | Position(Box, srcCoord), Position(Stock, destCoord) -> Position(Space, srcCoord), Position(StockedBox, destCoord)
                    | Position(StockedBox, srcCoord), Position(Space, destCoord) -> Position(Stock, srcCoord), Position(Box, destCoord)
                    | Position(StockedBox, srcCoord), Position(Stock, destCoord) -> Position(Stock, srcCoord), Position(StockedBox, destCoord)
                    | _ -> source, destination
            
                //printfn "try to move %A to %A" source destination
                match source, destination with
                | Some(s), Some(d) -> 
                    let newDestination, newSource = movePositions s d
                    if Position.haveSameCoord s newSource then
                        source, destination, game
                    else
                        //printfn "let's move!!"
                        Some(newDestination), Some(newSource), game |> Game.replace newSource |> Game.replace newDestination
                | _ -> source, destination, game

            static member play (move: Move) (game: Game) = 
                let human = Game.tryFindHuman game
                let humanTarget = Game.tryFindNextPosition move game human
                let boxTarget = Game.tryFindNextPosition move game humanTarget
                let newHumanTarget, _, tmpGame = game |> Game.tryMovePosition humanTarget boxTarget
                let _, _, finalGame = tmpGame |> Game.tryMovePosition human newHumanTarget
                finalGame