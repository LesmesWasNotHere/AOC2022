
bool InRange(int number, int[] i)
{
    return i[0] <= number && i[1] >= number;
}

var overlaps = File.ReadAllLines("input.txt")
    .Select(x => x.Split(",").Select(interval => interval.Split('-').Select(x => int.Parse(x)).ToArray()).ToArray())
    .Where(x => InRange(x[0][0], x[1]) || InRange(x[0][1], x[1]) || InRange(x[1][0], x[0]) || InRange(x[1][1], x[0]))
    .Count()
    ;

Console.WriteLine($"{overlaps} overlaps");
