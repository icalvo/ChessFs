namespace ChessFs.Api.Rest

open System.Text

open ChessFs.Chess
open Suave
open Suave.RequestErrors
open Suave.Operators
open Suave.Successful
open Suave.Filters
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

open ChessFs.Api.Payloads

type DatabaseError =
    | NotFound of int

type GameRepositoryError =
    | Database of DatabaseError
    | Validation of ValidationError

module DataBaseError =
    let message = function
        | NotFound id -> id |> sprintf "Game %i Not found"

module ValidationError =
    let private colorRepresentation = function
        | White -> "white"
        | Black -> "black"
    let message = function
    | Parsing msg -> msg |> sprintf "Parsing error: %s"
    | MoreThanOneKing color -> color |> colorRepresentation |> sprintf "There is more than one %s king"
    | MoreThanEightPawns color -> color |> colorRepresentation |> sprintf "There are more than eight %s pawns"
    | MoreExcessPiecesThanAbsentPawns color -> color |> colorRepresentation |> sprintf "There are more excess %s pieces than absent pawns"

module GameRepositoryError =
    let message = function
        | Database e -> DataBaseError.message e
        | Validation e -> ValidationError.message e

[<AutoOpen>]
module RestFul =
    type ChessStateRestResource = {
        Create : unit -> ChessPayload
        Play : int -> string -> Result<ChessPayload, GameRepositoryError>
        GetById : int -> Result<ChessPayload, DatabaseError>
        ItExists : int -> bool
    }

    let JSON v =
        let settings = JsonSerializerSettings()
        settings.ContractResolver <- CamelCasePropertyNamesContractResolver()

        JsonConvert.SerializeObject(v, settings)
        |> OK >=> Writers.setMimeType "application/json"

    let fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    let getResourceFromRequest<'a> (req: HttpRequest) =
        let getString (rawForm: byte array) = Encoding.UTF8.GetString rawForm
        req.rawForm |> getString |> fromJson<'a>

    let rest resourceName resource =
        let resourcePath = "/" + resourceName
        
        //let getAll = warbler (fun _ ->
        //    resource.GetAll () |> JSON)
        
        let badRequest = BAD_REQUEST

        let handleResource requestError = function
            | Ok r -> r |> JSON
            | Error msg -> requestError msg
        
        let resourceIdPath =
            let path = resourcePath + "/%d"
            PrintfFormat<int -> string, unit, string, string, int>(path)

        let resourceIdAndActionPath =
            let path = resourcePath + "/%d/%s"
            PrintfFormat<int -> string -> string, unit, string, string, (int * string)>(path)

        //let deleteResourceById id =
        //    resource.Delete id
        //    NO_CONTENT
        
        let getResourceById =
            resource.GetById >> Result.mapError DataBaseError.message >> handleResource NOT_FOUND

        let play args =
            args
            ||> resource.Play
            |> Result.mapError GameRepositoryError.message
            |> handleResource badRequest

        let resourceExists id =
            if resource.ItExists id then OK "" else NOT_FOUND ""
        let x = 1
        choose [
            path resourcePath >=> choose [
                // GET >=> getAll
                POST >=> (x |> JSON)
            ]
            // DELETE >=> pathScan resourceIdPath deleteResourceById
            GET >=> pathScan resourceIdPath getResourceById
            PUT >=> pathScan resourceIdAndActionPath play
            HEAD >=> pathScan resourceIdPath resourceExists
        ]
