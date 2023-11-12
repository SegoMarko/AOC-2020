module InputHelper

    let input sourceDirectory  : string seq = 
        System.IO.File.ReadAllLines $"{sourceDirectory}\\input.txt"

    let inputInt sourceDirectory  : int seq = 
        System.IO.File.ReadAllLines $"{sourceDirectory}\\input.txt" |> Seq.map int