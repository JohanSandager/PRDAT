using System;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        int[] xs = { 1, 3, 5 };
        int[] ys = { 2, 4, 6 };

        int[] result = merge(xs, ys);

        Console.WriteLine("Merged Array: " + string.Join(", ", result));
    }

    static int[] merge(int[] xs, int[] ys)
    {
        if (ys.Length == 0)
        {
            return xs;
        }
        else if (xs.Length == 0)
        {
            return ys;
        }

        var x = xs[0];
        var y = ys[0];
        if (x < y)
        {
            return new int[] { x }.Concat(merge(xs.Skip(1).ToArray(), ys)).ToArray();
        }
        else
        {
            return new int[] { y }.Concat(merge(xs, ys.Skip(1).ToArray())).ToArray();
        }
    }
}
