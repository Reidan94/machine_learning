namespace Digit_Recognition
{
  public class Observation
  {
    public string Label { get; private set; }
    public int[] Pixels { get; private set; }

    public Observation(string label, int[] pixels)
    {
      Label = label;
      Pixels = pixels;
    }
  }
}
