module InputHelper

    let input (day: int) : string seq = System.IO.File.ReadAllLines $".\\Day{day}\\input.txt"