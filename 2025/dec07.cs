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
    var (start, splitters) = Parse(input);
    Console.WriteLine($"a: {A(start, splitters)}");
    Console.WriteLine($"b: {B(start, splitters)}");
}

static int A((int, int) start, HashSet<(int, int)> splitters)
{
    var q = new Queue<(int, int)>([start]);
    var beams = new HashSet<(int, int)>();
    var bound = splitters.Max(p => p.Item2);
    var splits = 0;

    while (q.TryDequeue(out var p))
    {
        if (beams.Contains(p)) { continue; }
        if (p.Item2 > bound) { continue; }

        if (splitters.Contains(p))
        {
            splits += 1;
            q.Enqueue((p.Item1 - 1, p.Item2));
            q.Enqueue((p.Item1 + 1, p.Item2));
        }
        else
        {
            beams.Add(p);
            q.Enqueue((p.Item1, p.Item2 + 1));
        }
    }

    return splits;
}

static long B((int, int) start, HashSet<(int, int)> splitters)
{
    var seen = new Dictionary<(int, int), long>();
    var bound = splitters.Max(p => p.Item2);

    return TimelinesFrom(start, bound, splitters, seen);
}

static long TimelinesFrom((int, int) start, int bound, HashSet<(int, int)> splitters, Dictionary<(int, int), long> timelines)
{
    var (x, y) = start;

    while (y <= bound)
    {
        if (splitters.Contains((x, y)))
        {
            if (timelines.ContainsKey((x, y)))
            {
                return timelines[(x, y)];
            }

            timelines[(x, y)] = TimelinesFrom((x - 1, y + 1), bound, splitters, timelines) + TimelinesFrom((x + 1, y + 1), bound, splitters, timelines);

            return TimelinesFrom((x - 1, y), bound, splitters, timelines) + TimelinesFrom((x + 1, y), bound, splitters, timelines);
        }

        y++;
    }

    return 1;
}

static ((int, int), HashSet<(int, int)>) Parse(string input)
{
    var start = (0, 0);
    var splitters = new HashSet<(int, int)>();

    foreach (var (y, line) in input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries).Index())
    {
        foreach (var (x, c) in line.Index())
        {
            switch (c)
            {
                case 'S':
                    start = (x, y);
                    break;
                case '^':
                    splitters.Add((x, y));
                    break;
            }
        }
    }

    return (start, splitters);
}
