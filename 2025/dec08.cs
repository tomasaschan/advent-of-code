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
    var points = Parse(input);

    var (a, b) = Solve(points, Path.GetFileName(name) == "08.txt" ? 1000 : 10);
    Console.WriteLine($"a: {a}");
    Console.WriteLine($"b: {b}");
}

static (long, long) Solve(List<(long, long, long)> polongs, long linksToAdd)
{
    var circuits = polongs.Select((p, i) => (p, (long)i)).ToDictionary();
    var a = 0L;
    var linksAdded = 0;

    foreach ((_, var u, var v) in Distances(polongs).Order())
    {
        var (cu, cv) = (circuits[u], circuits[v]);
        linksAdded++;

        if (cu == cv)
        {
            continue;
        }

        foreach ((var p, var c) in circuits)
        {
            if (c == cv) { circuits[p] = cu; }
        }

        var sizes = CircuitSizes(circuits);

        if (linksAdded == linksToAdd)
        {
            a = sizes.Values.OrderDescending().Take(3).Aggregate((a, b) => a * b);
        }

        if (sizes.Keys.Distinct().Count() == 1)
        {
            var (x1, _, _) = u;
            var (x2, _, _) = v;

            return (a, x1 * x2);
        }
    }

    throw new InvalidProgramException();
}

static Dictionary<long, long> CircuitSizes(Dictionary<(long, long, long), long> circuits) => circuits.Values.GroupBy(id => id).ToDictionary(g => g.First(), g => (long)g.Count());

static IEnumerable<(double, (long, long, long), (long, long, long))> Distances(List<(long, long, long)> polongs)
{
    for (var i = 0; i < polongs.Count; i++)
    {
        var u = polongs[i];
        var (x1, y1, z1) = u;
        for (var j = i + 1; j < polongs.Count; j++)
        {
            var v = polongs[j];
            var (x2, y2, z2) = v;
            var distance = Math.Pow(x2 - x1, 2) + Math.Pow(y2 - y1, 2) + Math.Pow(z2 - z1, 2);
            yield return (distance, u, v);
        }
    }
}

static List<(long, long, long)> Parse(string input)
{
    return [.. input
        .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        .Select(line =>
        {
            return line.Split(",", StringSplitOptions.RemoveEmptyEntries) switch
            {
                [var x, var y, var z] => (long.Parse(x), long.Parse(y), long.Parse(z)),
                _ => throw new ArgumentException($"invalid input format; could not parse coordinate from line '{line}'")
            };
        })];
}
