open System
open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Newtonsoft.Json
// open Farmer
// open Farmer.Builders

// let myWebApp = webApp {
//     name "myFirstFarmerApp"
// }

// let deployment = arm {
//     location Location.NorthEurope
//     add_resource myWebApp
// }

// deployment
// |> Writer.quickWrite "myFirstTemplate"

// Define a record type to hold the data with a Shape field
type MantelRecord = {
    MDiameter: float
    PreisMantel: float
    Shape: string
    MWall: float
}

type KernRecord = {
    KDiameter: float
    PreisKern: float
}

type Shape = 
    | Round of MantelRecord
    | Square of MantelRecord

type CarbonValue = {
    CVMantelNormal: float
    CVKernNormal: float
    CVBetonNormal: float
    CVMantelGreen: float
    CVKernGreen: float
    CVBetonGreen: float
}

let carbonValues: CarbonValue list = [
    { CVMantelNormal = 2550.0; CVKernNormal = 2050.0; CVBetonNormal = 250.0; CVMantelGreen = 660.0; CVKernGreen = 470.0; CVBetonGreen = 160.0 }
]

// List of data records
let mantelData = [
    { MDiameter = 0.0; PreisMantel = 0.0; Shape = "Round"; MWall = 0.0 }
    { MDiameter = 100.0; PreisMantel = 1800.0; Shape = "Square"; MWall = 4.0 }
    { MDiameter = 101.6; PreisMantel = 1950.0; Shape = "Round"; MWall = 4.0 }
    { MDiameter = 120.0; PreisMantel = 1850.0; Shape = "Round"; MWall = 4.5 }
    { MDiameter = 121.0; PreisMantel = 1950.0; Shape = "Square"; MWall = 4.5 }
    { MDiameter = 127.0; PreisMantel = 1850.0; Shape = "Round"; MWall = 5.0 }
    { MDiameter = 133.0; PreisMantel = 1850.0; Shape = "Square"; MWall = 5.0 }
    { MDiameter = 151.0; PreisMantel = 1850.0; Shape = "Square"; MWall = 5.0 }

]

let kernData = [
    { KDiameter = 0.0; PreisKern = 0.0 }
    { KDiameter = 0.0; PreisKern = 2000.0 }
    { KDiameter = 40.0; PreisKern = 2000.0 }
    { KDiameter = 45.0; PreisKern = 1650.0 }
    { KDiameter = 50.0; PreisKern = 1650.0 }
    { KDiameter = 60.0; PreisKern = 1650.0 }
    { KDiameter = 70.0; PreisKern = 1650.0 }
]

// Volume calculation functions
let betonierungVolumenRound x y z q = 
    Math.PI * ((x - (2.0 * y)) ** 2.0 - z ** 2.0) * q / 4000000000.0

let betonierungVolumenSquare x y z q = 
    ((x - (2.0 * y)) ** 2.0 - Math.PI * (z / 2.0) ** 2.0) * q / 1000000000.0

// Function to determine and calculate volume based on shape
let betoVolumKalk (shape: Shape) (kern: KernRecord) (q: float) =
    match shape with
    | Round mantel -> betonierungVolumenRound mantel.MDiameter mantel.MWall kern.KDiameter q
    | Square mantel -> betonierungVolumenSquare mantel.MDiameter mantel.MWall kern.KDiameter q

// Weight calculations
let calculatePipeCrossSectionArea (mantel: MantelRecord) =
    let radiusOuter = mantel.MDiameter / 2.0 / 1000.0
    let radiusInner = radiusOuter - (mantel.MWall / 1000.0)
    let areaOuter = Math.PI * Math.Pow(radiusOuter, 2.0)
    let areaInner = Math.PI * Math.Pow(radiusInner, 2.0)
    areaOuter - areaInner

let calculateCoreCrossSectionArea (core: KernRecord) =
    let radiusCore = core.KDiameter / 2.0 / 1000.0
    let areaCore = Math.PI * Math.Pow(radiusCore, 2.0)
    areaCore

let calculateWeight x y =
    x * y * 7850.0

let calculatePipeWeight (mantel: MantelRecord) length =
    let crossSectionArea = calculatePipeCrossSectionArea mantel
    calculateWeight crossSectionArea length / 1000.0

let calculateCoreWeight (core: KernRecord) length =
    let crossSectionArea = calculateCoreCrossSectionArea core
    calculateWeight crossSectionArea length / 1000.0

let calculateConcreteWeightFromVolume x =
    x * 2400.0

let calculateSteelWeight x y = x + y

// Carbon emission calculations
let calculateCarbonEmissions pipeWeight coreWeight volume (carbonValues: CarbonValue list) =
    let CV = carbonValues.[0]
    let carbonEmissionsMantelNormal = CV.CVMantelNormal / 1000.0 * pipeWeight
    let carbonEmissionsMantelGreen = CV.CVMantelGreen / 1000.0 * pipeWeight
    let carbonEmissionsKernNormal = CV.CVKernNormal / 1000.0 * coreWeight
    let carbonEmissionsKernGreen = CV.CVKernGreen / 1000.0 * coreWeight
    let carbonEmissionsBetonNormal = CV.CVBetonNormal / 1000.0 * volume
    let carbonEmissionsBetonGreen = CV.CVBetonGreen / 1000.0 * volume
    let totalCarbonEmissionsNormal = carbonEmissionsMantelNormal + carbonEmissionsKernNormal + carbonEmissionsBetonNormal
    let totalCarbonEmissionsGreen = carbonEmissionsMantelGreen + carbonEmissionsKernGreen + carbonEmissionsBetonGreen
    totalCarbonEmissionsNormal, totalCarbonEmissionsGreen

// Function to query data based on user input
let queryData mantelValue kernValue =
    let mantelRecord = 
        mantelData
        |> List.tryFind (fun record -> record.MDiameter = mantelValue)
    
    let kernRecord = 
        kernData
        |> List.tryFind (fun record -> record.KDiameter = kernValue)

    match mantelRecord, kernRecord with
    | Some mRecord, Some kRecord -> 
        let mantelProduct = mRecord.MDiameter * mRecord.PreisMantel
        let kernProduct = kRecord.KDiameter * kRecord.PreisKern
        let totalProduct = mantelProduct + kernProduct
        Some (sprintf "Mantel: %f, Preis Mantel: %f, Shape: %s<br>Kern: %f, Preis Kern: %f<br>Mantel * Preis Mantel: %f, Kern * Preis Kern: %f, Total: %f" mRecord.MDiameter mRecord.PreisMantel mRecord.Shape kRecord.KDiameter kRecord.PreisKern mantelProduct kernProduct totalProduct)
    | _ -> None

// Define a record type for the JSON response
type CalculationResult = {
    Volume: float
    PipeWeight: float
    CoreWeight: float
    ConcreteWeight: float
    SteelWeight: float
    TotalCarbonEmissionsNormal: float
    TotalCarbonEmissionsGreen: float
}

// Define the web app
let webApp (logger: ILogger) =
    choose [
        route "/" >=> htmlFile "wwwroot/index.html"
        route "/query" >=> fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let mantelValue = ctx.Request.Query.["mantel"]
                let kernValue = ctx.Request.Query.["kern"]
                let lengthValue = ctx.Request.Query.["length"]
                logger.LogInformation($"Received parameters: mantelValue={mantelValue}, kernValue={kernValue}, length={lengthValue}")

                let mantelParsed, mantelValueParsed = Double.TryParse(mantelValue)
                let kernParsed, kernValueParsed = Double.TryParse(kernValue)
                let lengthParsed, lengthValueParsed = Double.TryParse(lengthValue)

                if mantelParsed && kernParsed && lengthParsed then
                    let mantelRecord = 
                        mantelData
                        |> List.tryFind (fun record -> record.MDiameter = mantelValueParsed)

                    match mantelRecord with
                    | Some mantel ->
                        let kern = { KDiameter = kernValueParsed; PreisKern = 0.0 }
                        let shape = if mantel.Shape = "Round" then Round mantel else Square mantel

                        let pipeWeight = calculatePipeWeight mantel lengthValueParsed
                        let coreWeight = calculateCoreWeight kern lengthValueParsed
                        let volume = betoVolumKalk shape kern lengthValueParsed
                        let concreteWeight = calculateConcreteWeightFromVolume volume
                        let steelWeight = calculateSteelWeight pipeWeight coreWeight

                        let totalCarbonEmissionsNormal, totalCarbonEmissionsGreen =
                            calculateCarbonEmissions pipeWeight coreWeight volume carbonValues

                        let result = {
                            Volume = volume
                            PipeWeight = pipeWeight
                            CoreWeight = coreWeight
                            ConcreteWeight = concreteWeight
                            SteelWeight = steelWeight
                            TotalCarbonEmissionsNormal = totalCarbonEmissionsNormal
                            TotalCarbonEmissionsGreen = totalCarbonEmissionsGreen
                        }

                        logger.LogInformation($"Query result: {JsonConvert.SerializeObject(result)}")
                        return! json result next ctx
                    | None ->
                        logger.LogInformation("No matching record found.")
                        let errorResult = {| error = "No matching record found." |}
                        return! json errorResult next ctx
                else
                    logger.LogInformation("Invalid input.")
                    let errorResult = {| error = "Invalid input. Please enter valid numbers for Mantel and Kern." |}
                    return! json errorResult next ctx
            }
    ]

// Configure services
let configureServices (services: IServiceCollection) =
    services.AddGiraffe() |> ignore
    services.AddLogging(fun builder ->
        builder.AddConsole() |> ignore
        builder.AddDebug() |> ignore) |> ignore

// Configure the HTTP request pipeline
let configureApp (app: IApplicationBuilder) =
    let logger = app.ApplicationServices.GetService<ILogger<obj>>()
    app.UseStaticFiles()
       .UseGiraffe(webApp logger)

// Configure and run the web host
[<EntryPoint>]
let main argv =
    Host.CreateDefaultBuilder(argv)
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder
                .ConfigureServices(configureServices)
                .Configure(configureApp)
                .UseUrls("http://localhost:5000") |> ignore)
        .Build()
        .Run()
    0
