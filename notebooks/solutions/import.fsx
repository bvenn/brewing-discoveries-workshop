#r "nuget: Deedle, 3.0.0"
#r "nuget: Deedle.Interactive, 3.0.0"

open Deedle

let df = Frame.ReadCsv($"{__SOURCE_DIRECTORY__}/../../data/coffeedata.txt", separators="\t")

type Category =
    | Coffee
    | Beer
    | Beverage
    | Deposit
    | TestStuff
    | Debit
    | Food
    | Milk
    | Hardware
    | Misc
    with 
        /// takes a string that describes the category and returns the corresponding category type
        static member fromString (s: string) =
            match s with
            | "Coffee" -> Coffee
            | "Beer" -> Beer
            | "Beverage" -> Beverage
            | "Deposit" -> Deposit
            | "TestStuff" -> TestStuff
            | "Debit" -> Debit
            | "Food" -> Food
            | "Milk" -> Milk
            | "Hardware" -> Hardware
            | "Misc" -> Misc
            | _ -> failwith "Unknown category"

type Order = {
    DateTime    : System.DateTime
    Name        : string
    Gender      : string
    Product     : string
    Price       : float
    Department  : string
    Category    : Category
    Amount      : int
    } with
        /// takes data row entities as input and creates a Order record type
        static member create time (name: string) gender product price department category amount = {
            DateTime  = time
            Name      = name
            Gender    = gender
            Product   = product
            Price     = price
            Department= department
            Category  = category
            Amount    = amount
            }

let orders = 
    df
    |> Frame.mapRows (fun key row ->
        {
            DateTime = System.DateTime.ParseExact((row.GetAs<string>("DateTime")),"dd/MM/yyyy HH:mm:ss",null)
            Name = row.GetAs<string>("Name")
            Gender = row.GetAs<string>("Gender")
            Product = row.GetAs<string>("Product")
            Price = row.GetAs<float>("Price")
            Department = row.GetAs<string>("Department")
            Category = row.GetAs<string>("Category") |> Category.fromString
            Amount = row.GetAs<int>("Amount")
        }
    )
    |> Series.values
    |> Array.ofSeq
    |> Array.sortBy (fun x -> x.DateTime)