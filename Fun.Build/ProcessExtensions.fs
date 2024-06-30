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

open System.Diagnostics
open System
open System.Runtime.InteropServices

#if WINDOWS
//    [<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = false)>]
//    extern IntPtr private SendMessage(IntPtr hWnd, UInt32 Msg, IntPtr wParam, IntPtr lParam);
//
//    [<Literal>]
//    let private WM_CLOSE = 0x10u;
//    let private terminate handle =
//        SendMessage(handle, WM_CLOSE, IntPtr.Zero, IntPtr.Zero) |> ignore
let private terminate (p: Process) = if p.CloseMainWindow() then Ok() else Error ""
#else
open Microsoft.FSharp.NativeInterop

[<DllImport("libc", SetLastError = true)>]
extern int private kill(int pid, int signal)

[<DllImport("libc", SetLastError = true)>]
extern int private strerror_r(int errnum, char* buf, UInt64 buflen)

let private getErrorMessage errno =
    let buffer = NativePtr.stackalloc<char> 1024
    let result = strerror_r (errno, buffer, 1024UL)

    if result = 0 then
        Marshal.PtrToStringAnsi(buffer |> NativePtr.toNativeInt)
    else
        $"errno %i{errno}"

[<Literal>]
let private SIGTERM = 15
[<Literal>]
let private ESRCH = 3
let private terminate pid =
    let code = kill (pid, SIGTERM)
    let errno = Marshal.GetLastWin32Error()
    if code = -1 && errno <> ESRCH then // ESRCH = process does not exist, assume it exited
        getErrorMessage errno |> Error
    else
        Ok code
#endif

let termProcess (p: Process) =
    if p.HasExited then
        Ok()
    else
#if WINDOWS
        match terminate p with
#else
        match terminate p.Id with
#endif
        | Ok _ -> Ok()
        | Error msg ->
            p.Kill()
            Error $"Unable to gracefully stop %s{p.ProcessName}. Killed process instead. %s{msg}"

let terminateProcess (p: Process) =
    match termProcess p with
    | Ok _ -> ()
    | Error error -> printfn "Failed shutting down process %d" p.Id

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

            //startInfo.CreateNoWindow <- true
            //startInfo.UseShellExecute <- true
            //startInfo.UseShellExecute
            use result = Process.Start startInfo
            printfn "id %d" result.Id
            //result.EnableRaisingEvents <- true
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

                    // result.Kill()
                    terminateProcess result
                //result.WaitForExit()
                )

            use _ =
                match cancellationToken with
                | Some ct ->
                    ct.Register(fun () ->
                        AnsiConsole.MarkupLine("[yellow]Command is cancelled by your token[/]")
                        terminateProcess result
                    //result.Kill()
                    // AnsiConsole.MarkupLine("[yellow]Waiting for exit[/]")
                    // while not result.HasExited && not result.Responding do
                    //     AnsiConsole.MarkupLine("[yellow]not exited[/]")
                    //     Thread.Sleep 1000
                    // // result.CancelOutputRead()
                    // // result.CancelErrorRead()
                    // result.WaitForExit()
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
