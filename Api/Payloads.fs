namespace ChessFs.Api

module Payloads =
    open ChessFs.Chess

    type ChessPayload = {
        Id: int
        State: ChessState
        Actions: string seq
    }

