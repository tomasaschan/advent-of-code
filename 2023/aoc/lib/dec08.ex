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

  def follow(instructions, map, pos, steps \\ 0)
  def follow(_, _, "ZZZ", steps), do: steps

  def follow(instructions, map, pos, steps) do
    instruction = instructions |> Enum.at(rem(steps, length(instructions)))
    {l, r} = map[pos]

    follow(
      instructions,
      map,
      case instruction do
        "L" -> l
        "R" -> r
      end,
      steps + 1
    )
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

    follow(instructions, map, "AAA")
  end

  def follow_all(instructions, maps, poss, steps \\ 0)
  def follow_all(_, _, _, 10), do: 10

  def follow_all(instructions, maps, poss, steps) do
    if poss |> Enum.all?(fn pos -> String.ends_with?(pos, "Z") end) do
      steps
    else
      nexts = poss |> Enum.map(fn pos -> follow(instructions, maps, pos, steps) end)
      IO.puts("nexts: #{inspect(nexts)}, steps: #{steps}")
      follow_all(instructions, maps, nexts, steps + 1)
    end
  end

  defmodule State do
    defstruct(pos: "AAA", iptr: 0, steps: 0)
  end

  defmodule Input do
    @enforce_keys [:map, :instructions]
    defstruct [:map, :instructions]
  end

  def step(%Input{map: map, instructions: instructions}, state) do
    instruction = instructions |> Enum.at(rem(state.iptr, length(instructions)))
    {l, r} = map[state.pos]

    %State{
      iptr: rem(state.iptr + 1, length(instructions)),
      steps: state.steps + 1,
      pos:
        case instruction do
          "L" -> l
          "R" -> r
        end
    }
  end

  def find_cycle(map, instructions, pos, seen \\ %{}, steps \\ 0, emit_seen \\ false) do
    iptr = rem(steps, length(instructions))

    cond do
      Map.has_key?(seen, {pos, iptr}) ->
        first = seen[{pos, iptr}]

        if emit_seen do
          {pos, first, steps, seen}
        else
          {pos, first, steps}
        end

      true ->
        nxt =
          step(%Input{map: map, instructions: instructions}, %State{
            pos: pos,
            iptr: iptr,
            steps: steps
          })

        find_cycle(
          map,
          instructions,
          nxt.pos,
          Map.put(seen, {pos, iptr}, steps),
          steps + 1,
          emit_seen
        )
    end
  end

  @doc """
  @tag timeout: :infinity
  iex>Dec08.b("LR
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

    follow_all(instructions, map, starts)
  end
end
