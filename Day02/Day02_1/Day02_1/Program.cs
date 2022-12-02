// A X Rock
// B Y Paper
// C Z Scissors

var winMoves = new HashSet<(char, char)>()
{
    ('A', 'B'),
    ('B', 'C'),
    ('C', 'A')
};

int score = File.ReadAllLines("input.txt")
    .Select(line => (line[0], (char)(line[2] - 23)))
    .Select(item =>
    {
        var score = item.Item2 - '@';

        if (item.Item1 == item.Item2)
            score += 3;
        else
            if (winMoves.Contains(item))
                score += 6;
        return score;

    })
    .Sum();

Console.WriteLine($"Score: {score}");
