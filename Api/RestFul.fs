namespace ChessFs.Api.Rest

open System.Text

open Suave
open Suave.RequestErrors
open Suave.Operators
open Suave.Successful
open Suave.Filters
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

open Engine
open ChessFs.Api.Payloads

[<AutoOpen>]
module RestFul =
    type ChessStateRestResource = {
        Create : unit -> ChessPayload
        Play : int -> string -> Result<ChessPayload, string>
        GetById : int -> Result<ChessPayload, string>
        ItExists : int -> bool
    }

    let JSON v =
        let settings = new JsonSerializerSettings ()
        settings.ContractResolver <- new CamelCasePropertyNamesContractResolver ()

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
            new PrintfFormat<(int -> string), unit, string, string, int>(path)

        let resourceIdAndActionPath =
            let path = resourcePath + "/%d/%s"
            new PrintfFormat<(int -> string -> string), unit, string, string, (int * string)>(path)

        //let deleteResourceById id =
        //    resource.Delete id
        //    NO_CONTENT
        
        let getResourceById =
            resource.GetById >> handleResource NOT_FOUND

        let play args =
            args
            ||> resource.Play
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
