using System;
using System.Collections.Generic;

namespace Digit_Recognition
{
  public class BaseClassifier : IClassifier
  {
    private IEnumerable<Observation> _data;
    private readonly IDistance _distance;

    public BaseClassifier(IDistance distance)
    {
      if (distance == null)
        throw new ArgumentNullException("distance");

      _distance = distance;
    }

    public void Train(IEnumerable<Observation> training)
    {
      _data = training;
    }

    public string Predict(int[] pixels)
    {
      if (_data == null)
        throw new InvalidOperationException("_data is not initialized!");

      string bestVal = null;
      var bestDistance = double.MaxValue;

      foreach (var observation in _data)
      {
        var distance = _distance.Between(observation.Pixels, pixels);
        if (distance < bestDistance)
        {
          bestVal = observation.Label;
          bestDistance = distance;
        }
      }

      return bestVal;
    }
  }
}
