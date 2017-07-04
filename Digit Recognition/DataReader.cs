using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Digit_Recognition
{
  public static class DataReader
  {
    private static Observation ObservationFactory(string data)
    {
      var array = data.Split(new[] { "," }, StringSplitOptions.RemoveEmptyEntries);
      return new Observation(array[0], array.Skip(2).Select(x => Convert.ToInt32(x)).ToArray());
    }

    public static IEnumerable<Observation> ReadFrom(string filePath)
    {
      return File.ReadAllLines(filePath).Skip(1).Select(ObservationFactory).ToList();      
    }
  }
}
