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
    var sequences = Parse(input).ToList();

    Console.WriteLine($"a: {sequences.SelectMany(r => InvalidIDs(r, IsTwiceRepeated)).Sum()}");
    Console.WriteLine($"b: {sequences.SelectMany(r => InvalidIDs(r, IsRepeated)).Distinct().Sum()}");
}

static bool IsTwiceRepeated(long id)
{
    var s = id.ToString();
    return s.Length % 2 == 0 && s[..(s.Length / 2)] == s[(s.Length / 2)..];
}

static bool IsRepeated(long id)
{
    var s = id.ToString();
    return (s + s)[1..^1].Contains(s, StringComparison.InvariantCulture);
}

static IEnumerable<long> InvalidIDs((long, long) range, Func<long, bool> isInvalid)
{
    var (a, b) = range;
    for (var n = a; n <= b; n++)
    {
        if (isInvalid(n)) { yield return n; }
    }
}

static IEnumerable<(long, long)> Parse(string input)
{
    return input.Split(",", StringSplitOptions.RemoveEmptyEntries)
    .Select(range =>
    {
        var parts = range.Split('-', StringSplitOptions.RemoveEmptyEntries);
        return (long.Parse(parts[0]), long.Parse(parts[1]));
    });
}
