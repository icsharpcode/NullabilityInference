using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Win32.SafeHandles;

namespace InferNull
{
    internal static class ProcessRunner
    {
        /// <param name="psi">Process is started from this information</param>
        /// <param name="stdOut">Defaults to Console.Out</param>
        /// <param name="stdErr">Defaults to Console.Error</param>
        internal static async Task<int> GetExitCodeAsync(this ProcessStartInfo psi, TextWriter? stdOut = null, TextWriter? stdErr = null)
        {
            stdOut ??= Console.Out;
            stdErr ??= Console.Error;
            psi.UseShellExecute = false;
            psi.RedirectStandardError = true;
            psi.RedirectStandardOutput = true;
            using var process = new Process() { StartInfo = psi };
            var stdOutComplete = new TaskCompletionSource<object?>();
            var stdErrComplete = new TaskCompletionSource<object?>();
            process.OutputDataReceived += (sender, e) => {
                if (e.Data != null)
                    stdOut.WriteLine(e.Data);
                else
                    stdOutComplete.SetResult(null);
            };
            process.ErrorDataReceived += (sender, e) => {
                if (e.Data != null)
                    stdErr.WriteLine(e.Data);
                else
                    stdErrComplete.SetResult(null);
            };
            try {
                process.Start();
            } catch (Win32Exception win32Exception) {
                await stdErr.WriteLineAsync(win32Exception.Message).ConfigureAwait(false);
                return win32Exception.ErrorCode;
            }
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();
            await Task.WhenAll(process.WaitForExitAsync(), stdOutComplete.Task, stdErrComplete.Task).ConfigureAwait(false);

            return process.ExitCode;
        }

        /// <summary>
        /// Asynchronously waits for the process to exit.
        /// </summary>
        public static Task WaitForExitAsync(this Process process)
        {
            if (process.HasExited)
                return Task.CompletedTask;
            var safeProcessHandle = process.SafeHandle;
            if (safeProcessHandle.IsClosed)
                throw new ObjectDisposedException("Process");
            var tcs = new TaskCompletionSource<object?>();
            var waitHandle = new ProcessWaitHandle(safeProcessHandle);
            RegisteredWaitHandle? registeredWaitHandle = null;
            lock (tcs) {
                registeredWaitHandle = ThreadPool.RegisterWaitForSingleObject(waitHandle, WaitForExitAsyncCallback, null, -1, true);
            }
            return tcs.Task;

            void WaitForExitAsyncCallback(object context, bool wasSignaled)
            {
                // The lock is used to ensure `registeredWaitHandle` is initialized here
                // even if the process terminates while `RegisterWaitForSingleObject` is returning.
                lock (tcs) {
                    registeredWaitHandle!.Unregister(null);
                }
                waitHandle.Close();
                tcs.SetResult(null);
            }
        }

        private sealed class ProcessWaitHandle : WaitHandle
        {
            public ProcessWaitHandle(SafeProcessHandle processHandle)
            {
                var currentProcess = new HandleRef(this, NativeMethods.GetCurrentProcess());
                SafeWaitHandle safeWaitHandle;
                if (!NativeMethods.DuplicateHandle(currentProcess, processHandle, currentProcess, out safeWaitHandle, 0, false, NativeMethods.DUPLICATE_SAME_ACCESS)) {
                    throw new Win32Exception();
                }
                base.SafeWaitHandle = safeWaitHandle;
            }
        }
        private static class NativeMethods
        {
            [DllImport("kernel32", SetLastError = true)]
            [return: MarshalAs(UnmanagedType.Bool)]
            internal static extern bool CloseHandle(IntPtr hObject);

            [DllImport("kernel32.dll")]
            internal static extern IntPtr GetCurrentProcess();

            [DllImport("kernel32.dll", BestFitMapping = false, CharSet = CharSet.Ansi)]
            internal static extern bool DuplicateHandle(HandleRef hSourceProcessHandle, SafeHandle hSourceHandle, HandleRef hTargetProcess, out SafeWaitHandle targetHandle, int dwDesiredAccess, bool bInheritHandle, int dwOptions);

            internal const int DUPLICATE_SAME_ACCESS = 2;
        }
    }
}