defmodule Dec22 do
  @doc """
  iex> "1,0,1~1,2,1
  ...>0,0,2~2,0,2
  ...>0,2,3~2,2,3
  ...>0,0,4~0,2,4
  ...>2,0,5~2,2,5
  ...>0,1,6~2,1,6
  ...>1,1,8~1,1,9
  ...>" |> Dec22.parse()
  [
    {{1,0,1}, {1,2,1}},
    {{0,0,2}, {2,0,2}},
    {{0,2,3}, {2,2,3}},
    {{0,0,4}, {0,2,4}},
    {{2,0,5}, {2,2,5}},
    {{0,1,6}, {2,1,6}},
    {{1,1,8}, {1,1,9}}
  ]
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [a, b] = line |> String.split("~", trim: true)

      point = fn str ->
        str |> String.split(",", trim: true) |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      end

      {point.(a), point.(b)}
    end)
  end

  defp sort(bricks),
    do:
      bricks
      |> Enum.sort_by(fn {{x1, y1, z1}, {x2, y2, z2}} ->
        {min(z1, z2), min(x1, x2), min(y1, y2)}
      end)

  defp lowered_to(z, brick) do
    {{x1, y1, z1}, {x2, y2, z2}} = brick
    {{x1, y1, z}, {x2, y2, z + abs(z2 - z1)}}
  end

  defp set_of(brick) do
    {{x1, y1, z1}, {x2, y2, z2}} = brick

    MapSet.new(
      for x <- x1..x2, y <- y1..y2, z <- z1..z2 do
        {x, y, z}
      end
    )
  end

  defp settle(brick, tower, z) do
    cond do
      z == 1 or MapSet.size(MapSet.intersection(tower, set_of(lowered_to(z - 1, brick)))) > 0 -> z
      true -> settle(brick, tower, z - 1)
    end
  end

  @doc """
  iex> [
  ...>   {{1,0,1}, {1,2,1}},
  ...>   {{0,0,2}, {2,0,2}},
  ...>   {{0,2,3}, {2,2,3}},
  ...>   {{0,0,4}, {0,2,4}},
  ...>   {{2,0,5}, {2,2,5}},
  ...>   {{0,1,6}, {2,1,6}},
  ...>   {{1,1,8}, {1,1,9}}
  ...> ] |> Dec22.settle()
  {
  [
    {{1,0,1}, {1,2,1}},
    {{0,0,2}, {2,0,2}},
    {{0,2,2}, {2,2,2}},
    {{0,0,3}, {0,2,3}},
    {{2,0,3}, {2,2,3}},
    {{0,1,4}, {2,1,4}},
    {{1,1,5}, {1,1,6}}
  ],
  5
  }
  iex> [
  ...>   {{0,0,2}, {2,0,2}}, # B
  ...>   {{0,2,2}, {2,2,2}}, # C
  ...>   {{0,0,3}, {0,2,3}}, # D
  ...>   {{2,0,3}, {2,2,3}}, # E
  ...>   {{0,1,4}, {2,1,4}}, # F
  ...>   {{1,1,5}, {1,1,6}}  # G
  ...> ] |> Dec22.settle() |> then(fn {_, moved} -> moved end)
  6
  iex> [
  ...>   {{1,0,1}, {1,2,1}}, # A
  ...>   {{0,2,2}, {2,2,2}}, # C
  ...>   {{0,0,3}, {0,2,3}}, # D
  ...>   {{2,0,3}, {2,2,3}}, # E
  ...>   {{0,1,4}, {2,1,4}}, # F
  ...>   {{1,1,5}, {1,1,6}}  # G
  ...> ] |> Dec22.settle() |> then(fn {_, moved} -> moved end)
  0
  iex> [
  ...>   {{1,0,1}, {1,2,1}}, # A
  ...>   {{0,0,2}, {2,0,2}}, # B
  ...>   {{0,0,3}, {0,2,3}}, # D
  ...>   {{2,0,3}, {2,2,3}}, # E
  ...>   {{0,1,4}, {2,1,4}}, # F
  ...>   {{1,1,5}, {1,1,6}}  # G
  ...> ] |> Dec22.settle() |> then(fn {_, moved} -> moved end)
  0
  iex> [
  ...>   {{1,0,1}, {1,2,1}}, # A
  ...>   {{0,0,2}, {2,0,2}}, # B
  ...>   {{0,2,2}, {2,2,2}}, # C
  ...>   {{2,0,3}, {2,2,3}}, # E
  ...>   {{0,1,4}, {2,1,4}}, # F
  ...>   {{1,1,5}, {1,1,6}}  # G
  ...> ] |> Dec22.settle() |> then(fn {_, moved} -> moved end)
  0
  iex> [
  ...>   {{1,0,1}, {1,2,1}}, # A
  ...>   {{0,0,2}, {2,0,2}}, # B
  ...>   {{0,2,2}, {2,2,2}}, # C
  ...>   {{0,0,3}, {0,2,3}}, # D
  ...>   {{0,1,4}, {2,1,4}}, # F
  ...>   {{1,1,5}, {1,1,6}}  # G
  ...> ] |> Dec22.settle() |> then(fn {_, moved} -> moved end)
  0
  iex> [
  ...>   {{1,0,1}, {1,2,1}}, # A
  ...>   {{0,0,2}, {2,0,2}}, # B
  ...>   {{0,2,2}, {2,2,2}}, # C
  ...>   {{0,0,3}, {0,2,3}}, # D
  ...>   {{2,0,3}, {2,2,3}}, # E
  ...>   {{1,1,5}, {1,1,6}}  # G
  ...> ] |> Dec22.settle() |> then(fn {_, moved} -> moved end)
  1
  iex> [
  ...>   {{1,0,1}, {1,2,1}}, # A
  ...>   {{0,0,2}, {2,0,2}}, # B
  ...>   {{0,2,2}, {2,2,2}}, # C
  ...>   {{0,0,3}, {0,2,3}}, # D
  ...>   {{2,0,3}, {2,2,3}}, # E
  ...>   {{0,1,4}, {2,1,4}}, # F
  ...> ] |> Dec22.settle() |> then(fn {_, moved} -> moved end)
  0
  """
  def settle(bricks) do
    bricks = sort(bricks)

    {_, result, moves} =
      bricks
      |> Enum.reduce({MapSet.new(), [], 0}, fn brick, {tower, result, moves} ->
        {{_, _, z1}, {_, _, z2}} = brick

        case {min(z1, z2), settle(brick, tower, min(z1, z2))} do
          {z0, z1} when z0 == z1 ->
            {MapSet.union(tower, set_of(brick)), [brick | result], moves}

          {_, z1} ->
            lowered = lowered_to(z1, brick)
            {MapSet.union(tower, set_of(lowered)), [lowered | result], moves + 1}
        end
      end)

    {sort(result), moves}
  end

  defp z_below(brick) do
    {{_, _, z1}, {_, _, z2}} = brick
    min(z1, z2) - 1
  end

  defp z_hi(brick) do
    {{_, _, z1}, {_, _, z2}} = brick
    max(z1, z2)
  end

  defp support(brick) do
    {{x1, y1, _}, {x2, y2, _}} = brick
    z = z_below(brick)

    MapSet.new(
      for x <- x1..x2, y <- y1..y2 do
        {x, y, z}
      end
    )
  end

  def find_disintegratable(bricks) do
    tower = bricks |> Enum.map(&set_of/1) |> Enum.reduce(&MapSet.union/2)

    bricks_by_z_below = bricks |> Enum.group_by(&z_below/1)

    bricks
    |> Enum.filter(fn brick ->
      z = z_hi(brick)

      tower_without = MapSet.difference(tower, set_of(brick))

      not (Map.get(bricks_by_z_below, z, [])
           |> Enum.any?(fn other ->
             MapSet.size(MapSet.intersection(support(other), tower_without)) == 0
           end))
    end)
  end

  @doc """
  iex> "1,0,1~1,2,1
  ...>0,0,2~2,0,2
  ...>0,2,3~2,2,3
  ...>0,0,4~0,2,4
  ...>2,0,5~2,2,5
  ...>0,1,6~2,1,6
  ...>1,1,8~1,1,9
  ...>" |> Dec22.a()
  5
  """
  def a(input) do
    input
    |> parse()
    |> settle()
    |> then(fn {result, _} -> result end)
    |> find_disintegratable()
    |> Enum.count()
  end

  def try_every_brick(bricks), do: try_every_brick(bricks, [], [])
  def try_every_brick([], _, results), do: results

  def try_every_brick([brick | rest], tried, results) do
    {_, moved} = settle(rest ++ tried)
    try_every_brick(rest, [brick | tried], [{brick, moved} | results])
  end

  @doc """
  iex> "1,0,1~1,2,1
  ...>0,0,2~2,0,2
  ...>0,2,3~2,2,3
  ...>0,0,4~0,2,4
  ...>2,0,5~2,2,5
  ...>0,1,6~2,1,6
  ...>1,1,8~1,1,9
  ...>" |> Dec22.b()
  7
  """
  def b(input) do
    input
    |> parse()
    |> sort()
    |> settle()
    |> then(fn {result, _} -> result end)
    |> try_every_brick()
    |> Enum.map(fn {_, moves} -> moves end)
    |> Enum.sum()
  end
end
