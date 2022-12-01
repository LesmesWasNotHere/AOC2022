

var lines = File.ReadAllLines("input");
var elfCalories = new List<int>();
var currentElfCalories = 0;

foreach (var line in lines)
{
    if (string.IsNullOrWhiteSpace(line))
    {
        elfCalories.Add(currentElfCalories);
        currentElfCalories = 0;
    }
    else
    {
        currentElfCalories += int.Parse(line);
    }
}

elfCalories.Add(currentElfCalories);

var top3ElfCalories = elfCalories.OrderByDescending(x => x).Take(3).Sum();

Console.WriteLine($"Top 3 elf calories: {top3ElfCalories}");