using System.Linq;

var badges = File.ReadLines("input.txt").ToArray().AsSpan<string>();
var priority = 0;

while (badges.Length > 0)
{
    var repeatedType = badges[..3].ToArray().Select(x => new HashSet<char>(x.ToCharArray())).Aggregate<HashSet<char>>((y, z) => y.Intersect(z).ToHashSet()).First();   
    priority += char.IsLower(repeatedType) ? repeatedType - 96 : repeatedType - 'A' + 27;
    badges = badges[3..];
}

Console.WriteLine($"Priority {priority}");