using System;
using System.Collections.Generic;
using System.Text;

namespace ICSharpCode.NullabilityInference
{
    public struct Statistics
    {
        public int NullableCount;
        public int NonNullCount;
        public int NotNullWhenCount;

        internal void Update(in Statistics s)
        {
            this.NullableCount += s.NullableCount;
            this.NonNullCount += s.NonNullCount;
            this.NotNullWhenCount += s.NotNullWhenCount;
        }
    }
}
