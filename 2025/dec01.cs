#!/usr/bin/env -S dotnet run

var inputs = Environment.GetCommandLineArgs() switch
{
    { Length: 0 } => throw new InvalidOperationException("No command line arguments found"),
    { Length: 1 } => [("stdin", Console.In.ReadToEnd())],
    { Length: > 1 }
arguments => arguments[1..].Select(a => (a, File.ReadAllText(a)))
};

foreach (var (name, input) in inputs)
{
    Solve(name, input);
}

static void Solve(string name, string input)
{
    Console.WriteLine($"--- {name} ---");

    var instructions = input
    .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    .Select(Parse)
    .ToList();

    Console.WriteLine(instructions.Aggregate(State.Initial, State.A).Password);
    Console.WriteLine(instructions.Aggregate(State.Initial, State.B).Password);
}

static Instruction Parse(string line)
{
    return line[0] switch
    {
        'L' => new(Direction.Left, int.Parse(line[1..])),
        'R' => new(Direction.Right, int.Parse(line[1..])),
        _ => throw new ArgumentException("line must start with L or R to be a valid instruction")
    };
}

enum Direction { Left, Right }

record Instruction(Direction Direction, int Magnitude);

record State(int DialState, int Password)
{

    public State ApplyA(Instruction instruction)
    {
        var (nxt, _) = Turn(instruction);
        return new(nxt, Password + (nxt == 0 ? 1 : 0));
    }

    public State ApplyB(Instruction instruction)
    {
        var (ns, zeroes) = Turn(instruction);
        return new(ns, Password + zeroes);
    }

    public (int, int) Turn(Instruction instruction)
    {
        var unmodulated = instruction.Direction switch
        {
            Direction.Left => DialState - (instruction.Magnitude % 100),
            Direction.Right => DialState + (instruction.Magnitude % 100),
            _ => throw new ArgumentOutOfRangeException(),
        };

        var laps = instruction.Magnitude / 100;

        var passesZero = (instruction.Direction, DialState, instruction.Magnitude % 100) switch
        {
            (Direction.Right, var s, var m) when s + m >= 100 => 1,
            (Direction.Left, var s, var m) when s != 0 && s - m <= 0 => 1,
            _ => 0
        };

        var result = (unmodulated + 100) % 100;

        return (result, passesZero + laps);
    }

    public static State Initial => new(50, 0);

    public static Func<State, Instruction, State> A => (state, instruction) => state.ApplyA(instruction);
    public static Func<State, Instruction, State> B => (state, instruction) => state.ApplyB(instruction);

    public override string ToString() => $"[{DialState:00}] ({Password:0})";
};
