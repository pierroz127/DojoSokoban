open System
open Sokoban.Game

let readFileName() = 
    let userInput = Console.ReadLine()
    if (String.IsNullOrEmpty userInput) then @"Example.txt" else userInput

type Command = 
    | Quit
    | Help
    | Reset
    | Move of Move

let parseCmd = 
    function
    | "up" | "haut" | "h" -> Move(Up)
    | "down" | "bas" |"b" -> Move(Down)
    | "right" | "droite" | "d" -> Move(Right)
    | "left" | "gauche" | "g" -> Move(Left)
    | "aide" | "help" | "?" -> Help 
    | "reset" -> Reset
    | "quit" -> Quit
    | _ -> failwith "Commande inconnue"

let printHelp() = 
    printfn @"Définition des symboles: 
- #: mur
- @: humain
- $: caisse
- .: emplacement pour les caisses
- +: humain sur un emplacement
- *: caisse sur un emplacement
======
Définition des commandes:
- gauche (ou g ou left): bouge l'humain vers la gauche
- droite (ou d ou right): bouge l'humain vers la droite
- haut (ou h ou up): bouge l'humain vers le haut
- bas (ou b ou down): bouge l'humain vers le bas
- quit: quitte le jeu
- aide (ou help ou ?): afficher l'aide"

let rec play (origin: Game) (game: Game)= 
    game |> Game.print
    try
        Console.Write("Commande: ")
        match Console.ReadLine().ToLowerInvariant() |> parseCmd with
        | Quit -> ()
        | Help -> printHelp(); play origin game 
        | Reset -> play origin origin
        | Move(move) -> game |> Game.play move |> play origin
    with 
    | _ -> play origin game

[<EntryPoint>]
let main args =
    Console.WriteLine("!!! Welcome to Sokoban !!!")
    Console.WriteLine("(tapez Aide ou ? pour afficher l'aide)")
    Console.Write("Fichier: ")
    let game = readFileName() |> Game.readFile
    play game game
    0