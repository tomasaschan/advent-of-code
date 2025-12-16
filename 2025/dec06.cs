#!/usr/bin/env -S dotnet run

using System.Diagnostics;

var inputs = Environment.GetCommandLineArgs() switch
{
    { Length: 0 } => throw new InvalidOperationException("No command line arguments found"),
    { Length: 1 } => [("stdin", Console.In.ReadToEnd())],
    { Length: > 1 } arguments => arguments[1..].Select(a => (a, File.ReadAllText(a)))
};

foreach (var (name, input) in inputs)
{
    Console.WriteLine($"--- {name} ---");
    Console.WriteLine($"a: {ParseA(input).Sum(p => p.Solve())}");
    Console.WriteLine($"b: {ParseB(input).Sum(p => p.Solve())}");
}

static IEnumerable<Problem> ParseA(string input)
{
    var operands = new List<LinkedList<long>>();

    foreach (var line in input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries))
    {
        Debug.WriteLine($"parsing line {line[..10]}...");
        Debug.Indent();
        var items = line.Split(" ", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);

        if (long.TryParse(items[0], out var _))
        {
            Debug.WriteLine("...as operands!");
            operands.Add(new LinkedList<long>(items.Select(long.Parse)));
            Debug.Unindent();
        }
        else
        {
            Debug.WriteLine("..as operators");
            Debug.Indent();
            foreach (var op in items)
            {
                Debug.WriteLine($"Considering {op} with operands [{string.Join(",", operands.Select(os => os.First?.Value))}]");
                var result = op switch
                {
                    "+" => new Problem(Operator.Add, [.. operands.Select(os => os.First?.Value ?? 0L)]),
                    "*" => new Problem(Operator.Multiply, [.. operands.Select(os => os.First?.Value ?? 1L)]),
                    _ => throw new NotImplementedException()
                };
                Debug.WriteLine($"Parsed as {result}");
                yield return result;
                operands.ForEach(os => os.RemoveFirst());
            }

            Debug.Unindent();
            yield break;
        }
    }

    throw new ArgumentException("input did not conform to expected format");
}

static IEnumerable<Problem> ParseB(string input)
{
    var tokens = input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries).Select(line => new LinkedList<char>(line)).ToList();

    Problem? current = null;

    foreach (var token in tokens[^1])
    {
        switch (token)
        {
            case '+':
                if (current is not null) { yield return current; }

                current = new Problem(Operator.Add, [long.Parse(string.Join("", tokens[..^1].Select(ts => ts.First?.Value)))]);
                tokens[..^1].ForEach(ts => ts.RemoveFirst());

                break;
            case '*':
                if (current is not null) { yield return current; }

                current = new Problem(Operator.Multiply, [long.Parse(string.Join("", tokens[..^1].Select(ts => ts.First?.Value)))]);
                tokens[..^1].ForEach(ts => ts.RemoveFirst());

                break;

            case ' ':
                if (current == null)
                {
                    throw new ArgumentException("input did not conform to expected format");
                }
                var digits = string.Join("", tokens[..^1].Select(ts => ts.First?.Value));
                tokens[..^1].ForEach(ts => ts.RemoveFirst());
                if (string.IsNullOrWhiteSpace(digits))
                {
                    yield return current;
                    current = null;
                    continue;
                }

                current.Operands.Add(long.Parse(digits));
                break;
        }
    }

    if (current is not null) { yield return current; }
}

enum Operator { Add, Multiply }

record Problem(Operator Operator, ICollection<long> Operands)
{
    public long Solve()
    {
        Debug.WriteLine($"Solving problem {this}");

        return Operator switch
        {
            Operator.Add => Operands.Sum(),
            Operator.Multiply => Operands.Aggregate((a, b) => a * b),
            _ => throw new NotImplementedException(),
        };
    }

    public override string ToString()
    {
        return string.Join($" {Operator switch { Operator.Add => "+", Operator.Multiply => "*", _ => throw new NotImplementedException() }} ", Operands.Reverse());
    }
}
