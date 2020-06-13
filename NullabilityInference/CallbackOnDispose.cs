using System;
using System.Threading;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Invokes an action when it is disposed.
    /// </summary>
    /// <remarks>
    /// This class ensures the callback is invoked at most once,
    /// even when Dispose is called on multiple threads.
    /// </remarks>
    public sealed class CallbackOnDispose : IDisposable
	{
        private Action? action;
		
		public CallbackOnDispose(Action action)
		{
            this.action = action ?? throw new ArgumentNullException(nameof(action));
		}
		
		public void Dispose()
		{
			Action? a = Interlocked.Exchange(ref action, null);
            a?.Invoke();
        }
	}
}
