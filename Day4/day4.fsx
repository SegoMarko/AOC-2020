#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

let input = InputHelper.input 4

let getPassports (sequence: seq<string>) =
    let passport = System.Collections.Generic.Dictionary<string, string>()
    seq {for line in sequence do
            if line = System.Environment.NewLine || line = "" then 
                yield passport
                passport.Clear()
            for part in line.Split(" ") do
                let keyValue = part.Split(":")
                passport.Add(keyValue[0], keyValue[1])}

let passports = getPassports input

let passportValid (passport: System.Collections.Generic.Dictionary<string, string>) =
    passport.ContainsKey "byr" &&
    passport.ContainsKey "iyr" &&
    passport.ContainsKey "eyr" &&
    passport.ContainsKey "hgt" &&
    passport.ContainsKey "hcl" &&
    passport.ContainsKey "ecl" &&
    passport.ContainsKey "pid"

let result1 = passports |> Seq.filter passportValid |> Seq.length
printfn $"{result1}"