[<AutoOpen>]
module Fun.Build.ProcessExtensions

open System
open System.IO
open System.Text
open System.Threading
open System.Diagnostics
open System.Runtime.InteropServices
open Spectre.Console
open Fun.Build.Internal

type Process with

    static member GetQualifiedFileName(cmd: string) =
        if
            not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            || Path.IsPathRooted(cmd)
            || not (String.IsNullOrWhiteSpace(Path.GetExtension cmd))
        then
            cmd
        else
            seq {
                use ps = Process.GetCurrentProcess()
                if ps.MainModule <> null then ps.MainModule.FileName

                Directory.GetCurrentDirectory()

                yield! windowsEnvPaths.Value()
            }
            |> Seq.tryPick (fun path ->
                windowsExeExts
                |> Seq.tryPick (fun ext ->
                    let file = Path.ChangeExtension(Path.Combine(path, cmd), ext)
                    if File.Exists file then Some file else None
                )
            )
            |> Option.defaultValue cmd


    static member StartAsync
        (
            startInfo: ProcessStartInfo,
            commandLogString: string,
            logPrefix: string,
            ?printOutput,
            ?captureOutput,
            ?cancellationToken: CancellationToken
        ) =
        async {
            let printOutput = defaultArg printOutput true
            let captureOutput = defaultArg captureOutput false
            let noPrefix = String.IsNullOrEmpty logPrefix
            // We want to redirect output if
            // 1. Use want to add prefix to the output
            // 2. User asked to not print output
            // 3. User asked to capture output
            let shouldRedirectOutput = not noPrefix || not printOutput || captureOutput

            // By default, we don't redirect output because redirecting the
            // output lose the color information.
            if shouldRedirectOutput then
                // We redirect both standard output and error output
                // because some process write mixed output to both...
                // This should in theory avoid losing information because of the redirection.
                startInfo.RedirectStandardOutput <- true
                startInfo.RedirectStandardError <- true
                startInfo.StandardOutputEncoding <- Encoding.UTF8
                startInfo.StandardErrorEncoding <- Encoding.UTF8

            startInfo.CreateNoWindow <- true
            startInfo.UseShellExecute <- true
            //startInfo.UseShellExecute
            use result = Process.Start startInfo
            result.EnableRaisingEvents <- true
            let standardOutputSb = StringBuilder()

            let handleDataReceived (ev: DataReceivedEventArgs) =
                if captureOutput then standardOutputSb.AppendLine ev.Data |> ignore
                if printOutput && not (String.IsNullOrEmpty ev.Data) then
                    if noPrefix then
                        Console.WriteLine(ev.Data)
                    else
                        Console.WriteLine(logPrefix + " " + ev.Data)

            if shouldRedirectOutput then
                result.OutputDataReceived.Add handleDataReceived
                result.ErrorDataReceived.Add handleDataReceived

            use! cd =
                Async.OnCancel(fun _ ->
                    AnsiConsole.Markup $"[yellow]{logPrefix}[/] "

                    AnsiConsole.WriteLine $"{commandLogString} is cancelled or timed out and the process will be killed."

                    result.Kill()
                    result.WaitForExit()
                )

            use _ =
                match cancellationToken with
                | Some ct ->
                    ct.Register(fun () ->
                        AnsiConsole.MarkupLine("[yellow]Command is cancelled by your token[/]")
                        result.Kill()
                        AnsiConsole.MarkupLine("[yellow]Waiting for exit[/]")
                        while not result.HasExited && not result.Responding do
                            AnsiConsole.MarkupLine("[yellow]not exited[/]")
                            Thread.Sleep 1000
                        // result.CancelOutputRead()
                        // result.CancelErrorRead()
                        result.WaitForExit()
                    // if (result.HasExited) then
                    //     AnsiConsole.MarkupLine("[yellow]exited[/]")
                    // else
                    //     AnsiConsole.MarkupLine("[yellow]not exited[/]")
                    )
                    :> IDisposable
                | _ ->
                    { new IDisposable with
                        member _.Dispose() = ()
                    }

            if shouldRedirectOutput then result.BeginOutputReadLine()

            result.WaitForExit()

            return struct {|
                ExitCode = result.ExitCode
                StandardOutput = standardOutputSb.ToString()
            |}
        }
