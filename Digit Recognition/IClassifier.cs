using System.Collections.Generic;

namespace Digit_Recognition
{
  public interface IClassifier
  {
    void Train(IEnumerable<Observation> training);
    string Predict(int[] pixels);
  }
}
