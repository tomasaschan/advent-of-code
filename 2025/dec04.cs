#!/usr/bin/env -S dotnet run

var inputs = Environment.GetCommandLineArgs() switch
{
    { Length: 0 } => throw new InvalidOperationException("No command line arguments found"),
    { Length: 1 } => [("stdin", Console.In.ReadToEnd())],
    { Length: > 1 } arguments => arguments[1..].Select(a => (a, File.ReadAllText(a)))
};

foreach (var (name, input) in inputs)
{
    Console.WriteLine($"--- {name} ---");
    var map = Parse(input);

    Console.WriteLine($"a: {Accessible(map).Count()}");
    Console.WriteLine($"b: {map.Count - Reduced(map).Count}");
}

static IEnumerable<(int, int)> Accessible(HashSet<(int, int)> map)
{
    foreach (var pos in map)
    {
        if (NeighborsOf(pos).Count(map.Contains) < 4)
        {
            yield return pos;
        }
    }
}

static HashSet<(int, int)> Reduced(HashSet<(int, int)> map)
{
    var accessible = Accessible(map).ToHashSet();

    if (accessible.Count == 0)
    {
        return map;
    }

    return Reduced([.. map.Except(accessible)]);
}

static IEnumerable<(int, int)> NeighborsOf((int x, int y) pos)
{
    for (var dx = -1; dx <= 1; dx++)
    {
        for (var dy = -1; dy <= 1; dy++)
        {
            if (dx == 0 && dy == 0) continue;
            yield return (pos.x + dx, pos.y + dy);
        }
    }
}

static HashSet<(int, int)> Parse(string input)
{
    return input
        .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        .SelectMany((line, y) => line.ToCharArray().Select((c, x) => (c, x, y)))
        .Where(t => { var (c, _, _) = t; return c == '@'; })
        .Select(t => { var (_, x, y) = t; return (x, y); })
        .ToHashSet();
}
