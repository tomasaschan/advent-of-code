defmodule Dec08 do
  def parse(input) do
    [instructions, paths] = String.split(input, "\n\n")

    map =
      paths
      |> String.split("\n", trim: true)
      |> Enum.map(fn line -> Regex.run(~r/(\w{3}) = \((\w{3}), (\w{3})\)/, line) end)
      |> Enum.map(fn [_, from, l, r] -> %{from => {l, r}} end)
      |> Enum.reduce(&Map.merge/2)

    {instructions |> String.split("", trim: true), map}
  end

  def done_a(pos), do: pos == "ZZZ"
  def done_b(pos), do: String.ends_with?(pos, "Z")

  def follow(instructions, map, pos, done, steps) do
    if done.(pos) do
      steps
    else
      follow(instructions, map, step(map, instructions, pos, steps), done, steps + 1)
    end
  end

  @doc """
  iex>Dec08.a("RL
  ...>
  ...>AAA = (BBB, CCC)
  ...>BBB = (DDD, EEE)
  ...>CCC = (ZZZ, GGG)
  ...>DDD = (DDD, DDD)
  ...>EEE = (EEE, EEE)
  ...>GGG = (GGG, GGG)
  ...>ZZZ = (ZZZ, ZZZ)
  ...>")
  2
  iex>Dec08.a("LLR
  ...>
  ...>AAA = (BBB, BBB)
  ...>BBB = (AAA, ZZZ)
  ...>ZZZ = (ZZZ, ZZZ)
  ...>")
  6
  """
  def a(input) do
    {instructions, map} = parse(input)

    follow(instructions, map, "AAA", &done_a/1, 0)
  end

  def step(map, instructions, pos, steps) do
    {l, r} = map[pos]

    case instructions |> Enum.at(rem(steps, length(instructions))) do
      "L" -> l
      "R" -> r
    end
  end

  def find_cycle(map, instructions, pos, seen \\ %{}, steps \\ 0, emit_seen \\ false) do
    iptr = rem(steps, length(instructions))

    cond do
      Map.has_key?(seen, {pos, iptr}) && done_b(pos) ->
        first = seen[{pos, iptr}]

        if emit_seen do
          {pos, first, steps, seen}
        else
          {pos, first, steps}
        end

      done_b(pos) ->
        find_cycle(
          map,
          instructions,
          step(map, instructions, pos, steps),
          Map.put(seen, {pos, iptr}, steps),
          steps + 1,
          emit_seen
        )

      true ->
        find_cycle(
          map,
          instructions,
          step(map, instructions, pos, steps),
          Map.put(seen, {pos, iptr}, steps),
          steps + 1,
          emit_seen
        )
    end
  end

  @doc """
  iex> Dec08.b("LR
  ...>
  ...>11A = (11B, XXX)
  ...>11B = (XXX, 11Z)
  ...>11Z = (11B, XXX)
  ...>22A = (22B, XXX)
  ...>22B = (22C, 22C)
  ...>22C = (22Z, 22Z)
  ...>22Z = (22B, 22B)
  ...>XXX = (XXX, XXX)
  ...>")
  6
  """
  def b(input) do
    {instructions, map} = parse(input)

    starts = map |> Map.keys() |> Enum.filter(fn k -> k |> String.ends_with?("A") end)

    starts
    |> Enum.map(fn start -> follow(instructions, map, start, &done_b/1, 0) end)
    |> Enum.reduce(&ElixirMath.lcm/2)
  end
end
