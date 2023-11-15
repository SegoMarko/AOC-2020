#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

let mutableDict () = new System.Collections.Generic.Dictionary<string, string>()
let input = InputHelper.input 4

let getPassports (sequence: seq<string>) =
    [let mutable passport = mutableDict ()
     for line in sequence do
            if line = "" then 
                yield passport
                passport <- mutableDict ()
            for part in line.Split(" ") do
                if part <> "" then
                    let keyValue = part.Split(":")
                    passport.Add(keyValue[0], keyValue[1])]

let yearValid lower upper (year: int) = year >= lower && year <= upper
let byrValid = yearValid 1920 2002
let iyrValid = yearValid 2010 2020
let eyrValid = yearValid 2020 2030
let hgtValid (hgt: string) =
    let hgtCheck = System.Text.RegularExpressions.Regex("^[1-9]{1}[0-9]{1,2}(cm|in)$")
    if hgtCheck.IsMatch hgt
    then let value = int (hgt |> Seq.take (hgt.Length - 2) |> System.String.Concat)
         let unit = hgt |> Seq.skip (hgt.Length - 2) |> System.String.Concat
         if unit = "cm"
         then value >= 150 && value <= 193
         else value >= 59 && value <= 76
    else false
let hclValid (hcl: string) = 
    let hclCheck = System.Text.RegularExpressions.Regex("^#[a-f0-9]{6}$")
    hclCheck.IsMatch hcl
let eclValid (ecl: string) =
    let eclCheck = System.Text.RegularExpressions.Regex("^(amb|blu|brn|gry|grn|hzl|oth)$")
    eclCheck.IsMatch ecl
let pidValid (pid: string) =
    let pidCheck = System.Text.RegularExpressions.Regex("^[0-9]{9}$")
    pidCheck.IsMatch pid

let passportValid (passport: System.Collections.Generic.Dictionary<string, string>) =
    passport.ContainsKey "byr" && 
    passport.ContainsKey "iyr" && 
    passport.ContainsKey "eyr" && 
    passport.ContainsKey "hgt" && 
    passport.ContainsKey "hcl" && 
    passport.ContainsKey "ecl" && 
    passport.ContainsKey "pid"

let passportReallyValid passport =
    passportValid passport &&
    byrValid (int passport["byr"]) &&
    iyrValid (int passport["iyr"]) &&
    eyrValid (int passport["eyr"]) &&
    hgtValid (passport["hgt"]) &&
    hclValid (passport["hcl"]) &&
    eclValid (passport["ecl"]) &&
    pidValid (passport["pid"])


let passports = getPassports input

let result1 = passports |> List.filter passportValid |> List.length
printfn $"{result1}"

let result2 = passports |> List.filter passportReallyValid |> List.length
printfn $"{result2}"