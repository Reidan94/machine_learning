using System.Collections.Generic;
using System.Linq;

namespace Digit_Recognition
{
  public static class Evaulator
  {
    public static double Validate(IEnumerable<Observation> observations, IClassifier classifier)
    {
      return observations.Select(o => classifier.Predict(o.Pixels) == o.Label ? 1.0 : 0.0).Average();
    }
  }
}
