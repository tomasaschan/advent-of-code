defmodule Dec10 do
  @doc """
  iex> Dec10.parse(".....\\n.F-7.\\n.|.|.\\n.L-J.\\n.....\\n")
  %{
    {1,1} => [:s, :e],
    {2,1} => [:w, :e],
    {3,1} => [:w, :s],
    {1,2} => [:n, :s],
    {3,2} => [:n, :s],
    {1,3} => [:n, :e],
    {2,3} => [:w, :e],
    {3,3} => [:w, :n],
  }
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.filter(fn
        {".", _} -> false
        _ -> true
      end)
      |> Enum.map(fn {char, x} ->
        case char do
          "F" -> {{x, y}, [:s, :e]}
          "-" -> {{x, y}, [:w, :e]}
          "|" -> {{x, y}, [:n, :s]}
          "L" -> {{x, y}, [:n, :e]}
          "J" -> {{x, y}, [:w, :n]}
          "7" -> {{x, y}, [:w, :s]}
          "S" -> {{x, y}, :start}
          true -> raise "Unknown char: #{char}"
        end
      end)
    end)
    |> Enum.into(%{})
  end

  def start(map) do
    map
    |> Enum.filter(fn
      {_, :start} -> true
      _ -> false
    end)
    |> Enum.map(fn {pos, _} -> pos end)
    |> List.first()
  end

  def toward(:n, {x, y}), do: {x, y - 1}
  def toward(:s, {x, y}), do: {x, y + 1}
  def toward(:e, {x, y}), do: {x + 1, y}
  def toward(:w, {x, y}), do: {x - 1, y}

  def explore(_, [], seen), do: seen

  def explore(map, [{pos, steps} | rest], seen) do
    case Map.get(seen, pos) do
      nil ->
        explore(
          map,
          rest ++
            (map[pos] |> Enum.map(fn dir -> {toward(dir, pos), steps + 1} end)),
          Map.put(seen, pos, steps)
        )

      _ ->
        explore(map, rest, seen)
    end
  end

  def start_and_neighbors(map) do
    start = start(map)

    neighbors =
      [
        {toward(:n, start), 1},
        {toward(:s, start), 1},
        {toward(:e, start), 1},
        {toward(:w, start), 1}
      ]
      |> Enum.filter(fn {pos, _} ->
        case Map.get(map, pos) do
          nil ->
            false

          nexts ->
            nexts
            |> Enum.map(fn dir ->
              toward(dir, pos)
            end)
            |> Enum.member?(start)
        end
      end)

    {start, neighbors}
  end

  @doc """
  iex> Dec10.a(".....\\n.S-7.\\n.|.|.\\n.L-J.\\n.....\\n")
  4
  iex> Dec10.a("-L|F7\\n7S-7|\\nL|7||\\n-L-J|\\nL|-JF\\n")
  4
  iex> Dec10.a("..F7.\\n.FJ|.\\nSJ.L7\\n|F--J\\nLJ...\\n")
  8
  """
  def a(input) do
    map = parse(input)
    {start, neighbors} = start_and_neighbors(map)

    explore(
      map,
      neighbors,
      %{start => 0}
    )
    |> Map.values()
    |> Enum.max()
  end

  def show(result) do
    IO.puts("")

    {min_x, max_x} =
      result
      |> Map.keys()
      |> Enum.map(fn {x, _} -> x end)
      |> Enum.min_max()

    {min_y, max_y} =
      result
      |> Map.keys()
      |> Enum.map(fn {_, y} -> y end)
      |> Enum.min_max()

    min_y..max_y
    |> Enum.map(fn y ->
      min_x..max_x
      |> Enum.map(fn x ->
        Map.get(result, {x, y})
        |> then(fn
          nil -> "."
          i -> Integer.to_string(i)
        end)
      end)
      |> Enum.join("")
    end)
    |> Enum.join("\n")
    |> IO.puts()
  end
end
