// A Rock       1
// B Paper      2
// C Scissor    3

// X Lose       0
// Y Draw       3
// Z Win        6

var resultBias = new Dictionary<char, int>()
{
    { 'X', 2 },
    { 'Y', 0 },
    { 'Z', 1 }
};

int score = File.ReadAllLines("input.txt")
    .Select(line => (line[0], line[2]))
    .Select(item => { 
            var result = ((item.Item1 - 'A') + resultBias[item.Item2]) % 3 + 1 + (item.Item2 - 'X') * 3; 
            Console.WriteLine($"{item}  {result}");
        return result;
    })
    .Sum();

Console.WriteLine($"Score: {score}");
