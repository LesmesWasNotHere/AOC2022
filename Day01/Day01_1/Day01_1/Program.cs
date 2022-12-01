
var lines = File.ReadAllLines("input");
var maxElfCalories = 0;
var currentElfCalories = 0;

foreach (var line in lines)
{
    if (string.IsNullOrWhiteSpace(line))
    {
        if (currentElfCalories > maxElfCalories)
            maxElfCalories = currentElfCalories;
        currentElfCalories = 0;
    }
    else
    {
        currentElfCalories += int.Parse(line);
    }
}

if(currentElfCalories > maxElfCalories)
    maxElfCalories = currentElfCalories;

Console.WriteLine($"Max elf calories: {maxElfCalories}");
