//8394
using System.Linq;

var priority = File.ReadLines("input.txt").Select(x =>
{
    var halfsize = x.Length / 2;
    var compartment1 = new HashSet<char>(x.AsSpan()[..halfsize].ToArray());
    var repeatedType = x.AsSpan()[halfsize..].ToArray().First(y => compartment1.Contains(y));
    return char.IsLower(repeatedType) ? repeatedType - 96 : repeatedType - 'A' + 27;
}).Sum();

Console.WriteLine($"Priority {priority}");