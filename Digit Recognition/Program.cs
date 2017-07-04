using System;

namespace Digit_Recognition
{
  static class Program
  {
    static void Main(string[] args)
    {
      var distance = new ManhatanDistance();
      var train = DataReader.ReadFrom(@"Data\trainingsample.csv");
      IClassifier classifier = new BaseClassifier(distance);
      classifier.Train(train);
      
      var validation = DataReader.ReadFrom(@"Data\validationsample.csv");
      var correctness = Evaulator.Validate(validation, classifier);
      Console.WriteLine("Correctens of classification algorithm - {0} %", correctness * 100);
      Console.ReadKey();
    }
  }
}
