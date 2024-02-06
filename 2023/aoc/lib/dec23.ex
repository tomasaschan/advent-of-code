defmodule Dec23 do
  defmodule Trails do
    defstruct [:map, :start, :finish]

    @doc """
    iex> "#.###
    ...>#....
    ...>#####" |> Dec23.parse()
    %{
      {0,0} => "#",
      {1,0} => ".",
      {2,0} => "#",
      {3,0} => "#",
      {4,0} => "#",
      {0,1} => "#",
      {1,1} => ".",
      {2,1} => ".",
      {3,1} => ".",
      {4,1} => ".",
      {0,2} => "#",
      {1,2} => "#",
      {2,2} => "#",
      {3,2} => "#",
      {4,2} => "#",
    }
    """
    def parse(input) do
      map =
        input
        |> String.split("\n", trim: true)
        |> Enum.with_index()
        |> Enum.flat_map(fn {line, y} ->
          line
          |> String.split("", trim: true)
          |> Enum.with_index()
          |> Enum.map(fn {char, x} -> {{x, y}, char} end)
        end)
        |> Enum.into(%{})

      yhi = map |> Map.keys() |> Enum.map(fn {_, y} -> y end) |> Enum.max()
      start = find_hole(map, 0)
      finish = find_hole(map, yhi)

      %Trails{map: map, start: start, finish: finish}
    end

    defp find_hole(map, y, x \\ 0) do
      case map[{x, y}] do
        "." -> {x, y}
        _ -> find_hole(map, y, x + 1)
      end
    end
  end

  defmodule SimpleMap do
    defstruct [:nodes, :start, :finish]
    defp up({x, y}), do: {{x, y - 1}, "^"}
    defp dn({x, y}), do: {{x, y + 1}, "v"}
    defp lt({x, y}), do: {{x - 1, y}, "<"}
    defp rt({x, y}), do: {{x + 1, y}, ">"}

    defp intersection?(map, [p | path], allow_uphill) do
      [up(p), dn(p), lt(p), rt(p)]
      |> Enum.count(fn {nxt, c} ->
        if Enum.member?(path, nxt) do
          true
        else
          case Map.get(map, nxt, "#") do
            "." -> true
            ^c -> true
            "#" -> false
            _ -> allow_uphill
          end
        end
      end) > 2
    end

    defp append_walkable(map, q, path, allow_uphill) do
      [p | _] = path

      [up(p), dn(p), lt(p), rt(p)]
      |> Enum.reject(fn {p, _} -> Enum.member?(path, p) end)
      |> Enum.filter(fn {p, c} ->
        case Map.get(map, p, "#") do
          "." -> true
          "#" -> false
          ^c -> true
          _ -> allow_uphill
        end
      end)
      |> Enum.reduce(q, fn {p, _}, q -> :queue.in([p | path], q) end)
    end

    defp add_neighbor(p, path, neighbors) do
      Map.update(neighbors, p, path, fn prev ->
        if Enum.count(path) > Enum.count(prev) do
          path
        else
          prev
        end
      end)
    end

    defp find_neighbors(trails, q, neighbors, allow_uphill) do
      finish = trails.finish

      case :queue.out(q) do
        {:empty, _} ->
          neighbors

        {{:value, [^finish | path]}, q} ->
          find_neighbors(trails, q, add_neighbor(finish, path, neighbors), allow_uphill)

        {{:value, [p]}, q} ->
          q = append_walkable(trails.map, q, [p], allow_uphill)
          find_neighbors(trails, q, neighbors, allow_uphill)

        {{:value, [p | path]}, q} ->
          if intersection?(trails.map, [p | path], allow_uphill) do
            find_neighbors(trails, q, add_neighbor(p, path, neighbors), allow_uphill)
          else
            q = append_walkable(trails.map, q, [p | path], allow_uphill)
            find_neighbors(trails, q, neighbors, allow_uphill)
          end
      end
    end

    def find_neighbors(trails, p, allow_uphill) do
      find_neighbors(trails, :queue.in([p], :queue.new()), %{}, allow_uphill)
    end

    defp simplify(trails, q, seen, nodes, allow_uphill) do
      finish = trails.finish

      case :queue.out(q) do
        {:empty, _} ->
          nodes

        {{:value, ^finish}, q} ->
          simplify(trails, q, seen, nodes, allow_uphill)

        {{:value, p}, q} ->
          if MapSet.member?(seen, p) do
            simplify(trails, q, seen, nodes, allow_uphill)
          else
            neighbors =
              find_neighbors(trails, p, allow_uphill)

            nodes = Map.put(nodes, p, neighbors)
            q = Enum.reduce(neighbors, q, fn {p, _}, q -> :queue.in(p, q) end)
            simplify(trails, q, MapSet.put(seen, p), nodes, allow_uphill)
          end
      end
    end

    def simplify(trails, allow_uphill) do
      q = :queue.in(trails.start, :queue.new())
      seen = MapSet.new()
      nodes = Map.new()

      nodes = simplify(trails, q, seen, nodes, allow_uphill)

      %SimpleMap{nodes: nodes, start: trails.start, finish: trails.finish}
    end
  end

  defp append_walkable(simple_map, q, {p, path}) do
    simple_map.nodes[p]
    |> Enum.reject(fn {nxt, _} ->
      nxt in (path |> Enum.map(fn {s, _} -> s end))
    end)
    |> Enum.reduce(q, fn {p, n}, q -> :queue.in({p, [{p, n} | path]}, q) end)
  end

  defp march(map, q, longest_known) do
    finish = map.finish

    case :queue.out(q) do
      {:empty, _} ->
        longest_known

      {{:value, {^finish, path}}, q} ->
        k =
          path
          |> Enum.map(fn {_, n} -> Enum.count(n) end)
          |> Enum.sum()

        n = longest_known |> Enum.map(fn {_, n} -> Enum.count(n) end) |> Enum.sum()

        march(
          map,
          q,
          if k > n do
            path
          else
            longest_known
          end
        )

      {{:value, next}, q} ->
        march(map, append_walkable(map, q, next), longest_known)
    end
  end

  def find_longest_path(map) do
    start = map.start

    q = :queue.in({start, [{start, []}]}, :queue.new())

    march(map, q, [])
  end

  @doc """
  iex> "#.#####################
  ...>#.......#########...###
  ...>#######.#########.#.###
  ...>###.....#.>.>.###.#.###
  ...>###v#####.#v#.###.#.###
  ...>###.>...#.#.#.....#...#
  ...>###v###.#.#.#########.#
  ...>###...#.#.#.......#...#
  ...>#####.#.#.#######.#.###
  ...>#.....#.#.#.......#...#
  ...>#.#####.#.#.#########v#
  ...>#.#...#...#...###...>.#
  ...>#.#.#v#######v###.###v#
  ...>#...#.>.#...>.>.#.###.#
  ...>#####v#.#.###v#.#.###.#
  ...>#.....#...#...#.#.#...#
  ...>#.#########.###.#.#.###
  ...>#...###...#...#...#.###
  ...>###.###.#.###v#####v###
  ...>#...#...#.#.>.>.#.>.###
  ...>#.###.###.#.###.#.#v###
  ...>#.....###...###...#...#
  ...>#####################.#
  ...>" |> Dec23.a()
  94
  """
  def a(input) do
    input |> solve(false)
  end

  @doc """
  iex> "#.#####################
  ...>#.......#########...###
  ...>#######.#########.#.###
  ...>###.....#.>.>.###.#.###
  ...>###v#####.#v#.###.#.###
  ...>###.>...#.#.#.....#...#
  ...>###v###.#.#.#########.#
  ...>###...#.#.#.......#...#
  ...>#####.#.#.#######.#.###
  ...>#.....#.#.#.......#...#
  ...>#.#####.#.#.#########v#
  ...>#.#...#...#...###...>.#
  ...>#.#.#v#######v###.###v#
  ...>#...#.>.#...>.>.#.###.#
  ...>#####v#.#.###v#.#.###.#
  ...>#.....#...#...#.#.#...#
  ...>#.#########.###.#.#.###
  ...>#...###...#...#...#.###
  ...>###.###.#.###v#####v###
  ...>#...#...#.#.>.>.#.>.###
  ...>#.###.###.#.###.#.#v###
  ...>#.....###...###...#...#
  ...>#####################.#
  ...>" |> Dec23.b()
  154
  """
  def b(input) do
    input |> solve(true)
  end

  defp solve(input, allow_uphill) do
    input
    |> Trails.parse()
    |> SimpleMap.simplify(allow_uphill)
    |> find_longest_path()
    |> Enum.map(fn {_, n} -> Enum.count(n) end)
    |> Enum.sum()
  end

  def show(map, path) do
    {xhi, yhi} =
      map
      |> Map.keys()
      |> Enum.reduce({0, 0}, fn {x, y}, {xhi, yhi} -> {max(xhi, x), max(yhi, y)} end)

    nodes =
      path
      |> Enum.map(fn {n, _} -> n end)
      |> Enum.reverse()
      |> Enum.with_index(1)
      |> Map.new()

    path =
      path
      |> Enum.flat_map(fn {_, path} -> path end)
      |> MapSet.new()

    for y <- 0..yhi do
      for x <- 0..xhi do
        cond do
          Map.has_key?(nodes, {x, y}) -> nodes[{x, y}]
          {x, y} in path -> "O"
          map[{x, y}] == "#" -> " "
          true -> map[{x, y}]
        end
      end
      |> Enum.join()
    end
    |> Enum.join("\n")
  end
end
