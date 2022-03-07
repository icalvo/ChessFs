namespace ChessFs.Api
open Engine

module Payloads =

    type ChessPayload = {
        Id: int
        State: ChessState
        Actions: string seq
    }

