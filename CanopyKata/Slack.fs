module Slack
open System.Net.WebSockets
open System.Threading
open System.IO
open FSharp.Data

type Channel = Channel of string
type Bot = Bot of string
type TeamMember = TeamMember of string
type Email = Email of string
type TextMessage = TextMessage of string

type private SlackUserId = SlackUserId of string
type private RtmStart = JsonProvider<"rtmStart.sample.json">
type private RtmMessage = JsonProvider<"rtmMessage.sample.json", SampleIsList=true>

let General = Channel "#general"

let [<Literal>] private token = "xoxb-62388246470-IeNmS3QrRntHNfUBmgdio13Y"

let post (Channel channel) text = 
        Http.AsyncRequest(
            "https://slack.com/api/chat.postMessage", 
            httpMethod = HttpMethod.Post,
            body = FormValues(
                [ ("token", token)
                  ("channel", channel)
                  ("text", text)
                  ("as_user", "true")
                  ("link_names", "1") ]))
        |> Async.Ignore