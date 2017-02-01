open System
open System.Collections.Generic

type Timed<'a> = 
    { Started : DateTimeOffset
      Stopped : DateTimeOffset
      Result : 'a}
    member this.Duration = this.Stopped - this.Started

type Queue = Queue of DateTimeOffset

let t = { 
    Started = DateTimeOffset(DateTime(2016, 12, 7), TimeSpan.FromHours 2.);
    Stopped = DateTimeOffset(DateTime(2016, 12, 8), TimeSpan.FromHours 2.);
    Result = 42}
t |> printfn "%A"

module Untimed = 
    let map f x = 
        {Started = x.Started; Stopped = x.Stopped; Result = f x.Result}
    
    let withResult newResult x = map (fun _ -> newResult) x


module Timed = 
    let capture clock x = 
        let now = clock() 
        {Started = now; Stopped = now; Result = x}

    let map clock f x =
        let result = f x.Result 
        let stopped = clock()
        {Started = x.Started; Stopped = stopped; Result = result }

    let timeOn clock f x = 
        x |> capture clock |> map clock f 

module Clocks = 
    let machineClock() = DateTimeOffset.Now
    machineClock() |> printfn "%A"

    let strTime (x : DateTimeOffset) = x.ToString "T"
    machineClock() |> strTime |> printfn "%A"

    let rnd = Random()

    let slowEcho x = 
        Async.Sleep(rnd.Next (500, 2000))
        |> Async.RunSynchronously
        x 

    let te : Timed<int> = Timed.timeOn machineClock slowEcho 42
    te |> printfn "%A"
    //time it takes to perform the slowEcho computation
    te.Duration.TotalMilliseconds |> printfn "%A"
    te.Duration.Milliseconds |> printfn "%A"

    //accelerated clock for simulations and prototyping
    let accClock (start : DateTimeOffset) rate () = 
        let now = DateTimeOffset.Now 
        let elapsed = now - start
        start.AddTicks (elapsed.Ticks * rate)
    
    let start = DateTimeOffset.Now
    let tenXclock = accClock start 10L
    tenXclock() |> strTime |> printfn "%A"
    let te' = Timed.timeOn tenXclock slowEcho 42
    te'.Duration.TotalMilliseconds |> printfn "%A"

    //fake clocks for unit-testing purposes or queues
    //Here, we need Collections so we can use a 'q of t'
    let qlock (q : Queue<DateTimeOffset>) = q.Dequeue
    //in order to create a queue, we need a sequence of DateTimeOffset values
    let now = DateTimeOffset.Now
    let dts = 
        Seq.init 10 float
        |> Seq.map (fun x -> now + TimeSpan.FromMinutes x)
        |> Seq.map strTime
        |> Seq.toList
        |> printfn "%A" 
    
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

