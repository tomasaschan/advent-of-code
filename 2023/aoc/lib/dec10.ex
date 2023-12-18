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
    start = start(map)

    explore(Map.put(map, start, deduce_start_pipe_type(start, map)), [{start, 0}], %{})
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

  @doc """
  iex> Dec10.b("...........
  ...>.S-------7.
  ...>.|F-----7|.
  ...>.||.....||.
  ...>.||.....||.
  ...>.|L-7.F-J|.
  ...>.|..|.|..|.
  ...>.L--J.L--J.
  ...>...........")
  4

  iex> Dec10.b(".F----7F7F7F7F-7....
  ...>.|F--7||||||||FJ....
  ...>.||.FJ||||||||L7....
  ...>FJL7L7LJLJ||LJ.L-7..
  ...>L--J.L7...LJS7F-7L7.
  ...>....F-J..F7FJ|L7L7L7
  ...>....L7.F7||L7|.L7L7|
  ...>.....|FJLJ|FJ|F7|.LJ
  ...>....FJL-7.||.||||...
  ...>....L---J.LJ.LJLJ...")
  8

  """
  def b(input) do
    map = parse(input)
    start = start(map)
    map = Map.put(map, start, deduce_start_pipe_type(start, map))
    tunnels = explore(map, [{start, 0}], %{})

    {xs, ys} =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(fn line -> String.length(line) end)
      |> Enum.reduce({0, 0}, fn x, {xs, ys} -> {max(x, xs), ys + 1} end)

    0..ys
    |> Enum.map(fn y ->
      0..xs
      |> Enum.map(fn x ->
        if Map.has_key?(tunnels, {x, y}) do
          Map.get(map, {x, y}, [])
          |> then(fn ends -> {x, y, ends |> Enum.member?(:n), ends |> Enum.member?(:s)} end)
        else
          {x, y, false, false}
        end
      end)
      |> Enum.reduce({{MapSet.new(), false}, {MapSet.new(), false}}, fn {x, y, crosses_north,
                                                                         crosses_south},
                                                                        {{inside_n, is_inside_n},
                                                                         {inside_s, is_inside_s}} ->
        {case {is_inside_n, crosses_north} do
           {true, true} -> {inside_n, false}
           {true, false} -> {MapSet.put(inside_n, {x, y}), true}
           {false, true} -> {inside_n, true}
           {false, false} -> {inside_n, false}
         end,
         case {is_inside_s, crosses_south} do
           {true, true} -> {inside_s, false}
           {true, false} -> {MapSet.put(inside_s, {x, y}), true}
           {false, true} -> {inside_s, true}
           {false, false} -> {inside_s, false}
         end}
      end)
    end)
    |> Enum.map(fn {{inside_n, _}, {inside_s, _}} -> MapSet.intersection(inside_n, inside_s) end)
    |> Enum.reduce(MapSet.new(), &MapSet.union/2)
    |> then(&MapSet.size/1)
  end

  def deduce_start_pipe_type(start, map) do
    [{:n, :s}, {:s, :n}, {:e, :w}, {:w, :e}]
    |> Enum.filter(fn {t, b} -> Map.get(map, toward(t, start), []) |> Enum.member?(b) end)
    |> Enum.map(fn {t, _} -> t end)
  end

  def debug_b(input, result) do
    {xs, ys} =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(fn line -> String.length(line) end)
      |> Enum.reduce({0, 0}, fn x, {xs, ys} -> {max(x, xs), ys + 1} end)

    map = parse(input)

    0..ys
    |> Enum.map(fn y ->
      0..xs
      |> Enum.map(fn x ->
        cond do
          Map.has_key?(result, {x, y}) -> "I"
          Map.get(map, {x, y}, []) -> "#"
          true -> "."
        end
      end)
      |> Enum.join()
    end)
    |> Enum.join("\n")
    |> IO.puts()

    0
  end
end
