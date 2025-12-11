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

    Console.WriteLine($"a: {Parse(input).Sum(bank => MaxJoltage(bank, 2))}");
    Console.WriteLine($"b: {Parse(input).Sum(bank => MaxJoltage(bank, 12))}");
}

static long MaxJoltage(List<int> bank, int batteries)
{
    var best = new int[batteries];

    for (var i = 0; i < bank.Count; i++)
    {
        var battery = bank[i];
        for (var k = 0; k < batteries; k++)
        {
            if (battery > best[k] && (bank.Count - i) > (batteries - k - 1))
            {
                best[k] = battery;
                for (var j = k + 1; j < batteries; j++) { best[j] = 0; }
                break;
            }
        }
    }


    return best.Select((j, i) => Math.Pow(10, batteries - i - 1) * j).Sum(i => (long)i);
}

static IEnumerable<List<int>> Parse(string input)
{
    return input
        .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        .Select(line => line.ToCharArray().Select(c => int.Parse(c.ToString())).ToList());
}
