
bool Overlaps(int[] i1, int[] i2)
{
    return i1[0] <= i2[0] && i1[1] >= i2[1];
}

var overlaps = File.ReadAllLines("input.txt")
    .Select(x => x.Split(",").Select(interval => interval.Split('-').Select(x => int.Parse(x)).ToArray()).ToArray())
    .Where(x => Overlaps(x[0], x[1]) || Overlaps(x[1], x[0]))
    .Count()
    ;

Console.WriteLine($"{overlaps} overlaps");

