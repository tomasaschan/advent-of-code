defmodule Dec21 do
  require Integer

  defmodule InfiniteMap do
    defstruct map: %{}, xhi: 0, yhi: 0, start: nil

    def parse(input) do
      input
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {line, y} ->
        line
        |> String.split("", trim: true)
        |> Enum.with_index()
        |> Enum.map(fn {c, x} -> {{x, y}, c} end)
      end)
      |> to_map_with_size({Map.new(), 0, 0, nil})
      |> then(fn {map, xhi, yhi, start} ->
        %InfiniteMap{map: map, xhi: xhi, yhi: yhi, start: start}
      end)
    end

    defp to_map_with_size([], {map, xhi, yhi, start}), do: {map, xhi, yhi, start}

    defp to_map_with_size([{{x, y}, "S"} | rest], {map, xhi, yhi, nil}) do
      to_map_with_size(rest, {
        Map.put(map, {x, y}, "."),
        max(x, xhi),
        max(y, yhi),
        {x, y}
      })
    end

    defp to_map_with_size([{{x, y}, c} | rest], {map, xhi, yhi, start}) do
      to_map_with_size(
        rest,
        {Map.put(map, {x, y}, c), max(x, xhi), max(y, yhi), start}
      )
    end

    @doc """
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({0, 0})
    "a"
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({1, 0})
    "b"
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({0, 1})
    "c"
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({1, 1})
    "d"
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({2, 0})
    "a"
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({0, 2})
    "a"
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({-1, 0})
    "b"
    iex> %Dec21.InfiniteMap{map: %{{0, 0} => "a", {1, 0} => "b", {0, 1} => "c", {1, 1} => "d"}, xhi: 1, yhi: 1} |> Dec21.InfiniteMap.at({0, -1})
    "c"
    """
    def at(%InfiniteMap{map: map, xhi: xhi, yhi: yhi}, {x, y}) do
      map[{Integer.mod(x, xhi + 1), Integer.mod(y, yhi + 1)}]
    end

    defimpl Inspect, for: InfiniteMap do
      def inspect(%InfiniteMap{map: map, xhi: xhi, yhi: yhi}, _) when xhi < 50 and yhi < 50 do
        "\n" <>
          (for y <- 0..yhi do
             for x <- 0..xhi do
               map[{x, y}]
             end
             |> Enum.join()
           end
           |> Enum.join("\n"))
      end

      def inspect(%InfiniteMap{map: _, xhi: xhi, yhi: yhi, start: start}, _) do
        "InfiniteMap of size [0,#{xhi}] x [0,#{yhi}] with start at #{inspect(start)}"
      end
    end

    def show_reachable_in(map, seen, n) do
      {xlo, xhi, ylo, yhi} =
        seen
        |> MapSet.to_list()
        |> Enum.reduce({0, map.xhi, 0, map.yhi}, fn {{x, y}, _}, {xlo, xhi, ylo, yhi} ->
          {min(x, xlo), max(x, xhi), min(y, ylo), max(y, yhi)}
        end)

      for y <- ylo..yhi do
        for x <- xlo..xhi do
          if {x, y} == map.start do
            "S"
          else
            case {at(map, {x, y}), MapSet.member?(seen, {{x, y}, Integer.is_even(n)})} do
              {"#", false} -> "#"
              {".", false} -> "."
              {".", true} -> "O"
            end
          end
        end
      end
      |> Enum.join("\n")
    end
  end

  defmodule Naive do
    defp above({x, y}), do: {x, y - 1}
    defp below({x, y}), do: {x, y + 1}
    defp left({x, y}), do: {x - 1, y}
    defp right({x, y}), do: {x + 1, y}

    defp append_walkable(map, q, p, s) do
      [&above/1, &below/1, &left/1, &right/1]
      |> Enum.map(fn f -> f.(p) end)
      |> Enum.filter(fn p -> InfiniteMap.at(map, p) != "#" end)
      |> Enum.reduce(q, fn p, q -> :queue.in({p, s + 1}, q) end)
    end

    defp march(map, q, seen, n) do
      case :queue.out(q) do
        {:empty, _} ->
          seen

        {{:value, {_, s}}, q} when s > n ->
          march(map, q, seen, n)

        {{:value, {p, s}}, q} ->
          {q, seen} =
            if MapSet.member?(seen, {p, Integer.is_even(s)}) do
              {q, seen}
            else
              {
                append_walkable(map, q, p, s),
                MapSet.put(seen, {p, Integer.is_even(s)})
              }
            end

          march(map, q, seen, n)
      end
    end

    def explore(map, n) do
      q = :queue.in({map.start, 0}, :queue.new())
      seen = MapSet.new([{map.start, 0}])

      march(map, q, seen, n)
    end
  end

  def count_reachable(seen, steps) do
    seen |> MapSet.filter(fn {_, even} -> even == Integer.is_even(steps) end) |> MapSet.size()
  end

  @doc """
  iex> "...........
  ...>.....###.#.
  ...>.###.##..#.
  ...>..#.#...#..
  ...>....#.#....
  ...>.##..S####.
  ...>.##..#...#.
  ...>.......##..
  ...>.##.#.####.
  ...>.##..##.##.
  ...>...........
  ...>" |> Dec21.a(6)
  16
  iex> "...........
  ...>.....###.#.
  ...>.###.##..#.
  ...>..#.#...#..
  ...>....#.#....
  ...>.##..S####.
  ...>.##..#...#.
  ...>.......##..
  ...>.##.#.####.
  ...>.##..##.##.
  ...>...........
  ...>" |> Dec21.a(10)
  50
  iex> "...........
  ...>.....###.#.
  ...>.###.##..#.
  ...>..#.#...#..
  ...>....#.#....
  ...>.##..S####.
  ...>.##..#...#.
  ...>.......##..
  ...>.##.#.####.
  ...>.##..##.##.
  ...>...........
  ...>" |> Dec21.a(50)
  1594
  """
  def a(input, steps \\ 64) do
    map =
      input
      |> InfiniteMap.parse()

    Naive.explore(map, steps)
    |> count_reachable(steps)
  end

  @doc """
  iex> "...........
  ...>.....###.#.
  ...>.###.##..#.
  ...>..#.#...#..
  ...>....#.#....
  ...>.##..S####.
  ...>.##..#...#.
  ...>.......##..
  ...>.##.#.####.
  ...>.##..##.##.
  ...>...........
  ...>" |> Dec21.b(115)
  """
  def b(input, n \\ 26_501_365) do
    map = input |> InfiniteMap.parse()

    # solution only valid for square grids with starting point in the center
    l = map.xhi + 1
    ^l = map.yhi + 1
    {m, m} = map.start
    2 = div(l, m)
    1 = rem(l, m)

    # solution only valid if n steps take us right to the edge of the map,
    # at some number of repeats away from the center
    ^m = rem(n, l)

    # build a system of equations for a*x^2 + b*x + c = y
    # where x is the number of steps and y the number of reachable plots
    {a, b} =
      1..3
      |> Enum.map(fn k -> {k, m + k * (map.xhi + 1)} end)
      |> Enum.map(fn {k, s} -> {k, Naive.explore(map, s) |> count_reachable(s)} end)
      |> Enum.map(fn {x, y} ->
        {[x * x, x, 1], y}
      end)
      |> Enum.reduce({[], []}, fn {xs, y}, {a, b} -> {a ++ [xs], b ++ [[y]]} end)

    # solve the system of equations to obtain the coefficients of the quadratic equation
    s = div(n, l)

    [a, b, c] =
      Matrix.inv(a)
      |> Matrix.mult(b)
      |> Enum.flat_map(&Function.identity/1)

    # insert the target number of full repeats
    round(a) * s * s + round(b) * s + round(c)
  end
end
