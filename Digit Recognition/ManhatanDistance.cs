using System;

namespace Digit_Recognition
{
  public class ManhatanDistance : IDistance
  {
    public double Between(int[] x1, int[] x2)
    {
      var rez = 0d;
      for (var i = 0; i < Math.Min(x1.Length, x2.Length); i++)
        rez += Math.Abs(x1[i] - x2[i]);

      return rez;
    }
  }
}
