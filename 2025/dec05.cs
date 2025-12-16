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

    var (ranges, ingredients) = Parse(input);

    Console.WriteLine($"a: {FreshIngredients(ranges, ingredients).Count()}");
    Console.WriteLine($"b: {ranges.WithoutOverlaps().Sum(r => r.Length())}");
}

static IEnumerable<long> FreshIngredients(IEnumerable<(long, long)> ranges, IEnumerable<long> ingredients)
{
    var fresh = new LinkedList<(long, long)>(ranges.WithoutOverlaps());

    foreach (var ingredient in ingredients.Order())
    {
        while (fresh.First is not null && fresh.First.Value.Item2 < ingredient)
        {
            fresh.RemoveFirst();
        }
        if (fresh.First is null) { yield break; }

        var current = fresh.First;

        if (current.Value.Item1 <= ingredient && ingredient <= current.Value.Item2)
        {
            yield return ingredient;
        }
    }
}

static (IEnumerable<(long, long)>, IEnumerable<long>) Parse(string input)
{
    var sections = input.Split(Environment.NewLine + Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

    var ranges = sections[0]
        .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        .Select(line =>
        {
            var parts = line.Split('-', StringSplitOptions.RemoveEmptyEntries);
            return (long.Parse(parts[0]), long.Parse(parts[1]));
        });

    var numbers = sections[1]
        .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        .Select(line => long.Parse(line));

    return (ranges, numbers);
}

static class Extensions
{
    public static IEnumerable<(long, long)> WithoutOverlaps(this IEnumerable<(long, long)> ranges)
    {
        var current = (long.MinValue, long.MinValue);

        foreach (var range in ranges.Order())
        {
            if (current.Item2 < range.Item1)
            // current is entirely before range
            // yield as-is and move on
            {
                if (current.Item1 != long.MinValue && current.Item2 != long.MinValue) { yield return current; }
                current = range;
            }
            else
            // current ends after range starts
            // merge them
            {
                current = (current.Item1, Math.Max(current.Item2, range.Item2));
            }
        }

        yield return current;
    }

    public static long Length(this (long, long) range) => range.Item2 - range.Item1 + 1;
}
