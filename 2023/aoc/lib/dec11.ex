defmodule Dec11 do
  @doc """
  iex> Dec11.parse("...#......
  ...>.......#..
  ...>#.........
  ...>..........
  ...>......#...
  ...>.#........
  ...>.........#
  ...>..........
  ...>.......#..
  ...>#...#.....")
  MapSet.new([
    {3,0},
    {7,1},
    {0,2},
    {6,4},
    {1,5},
    {9,6},
    {7,8},
    {0,9},
    {4,9}
  ])
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {line, y} ->
      line
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> char == "#" end)
      |> Enum.map(fn {_, x} -> {x, y} end)
    end)
    |> List.flatten()
    |> MapSet.new()
  end

  def size(space) do
    space
    |> Enum.reduce({0, 0}, fn {x, y}, {max_x, max_y} ->
      {max(max_x, x), max(max_y, y)}
    end)
  end

  @doc """
  iex> Dec11.expand(MapSet.new([{3,0},{7,1},{0,2},{6,4},{1,5},{9,6},{7,8},{0,9},{4,9}]))
  Dec11.parse("....#........
  .........#...
  #............
  .............
  .............
  ........#....
  .#...........
  ............#
  .............
  .............
  .........#...
  #....#.......")
  """
  def expand(space, factor \\ 2) do
    {max_x, max_y} = size(space)

    columns =
      0..max_x
      |> Enum.filter(fn x ->
        0..max_y |> Enum.all?(fn y -> not MapSet.member?(space, {x, y}) end)
      end)
      |> MapSet.new()

    rows =
      0..max_y
      |> Enum.filter(fn y ->
        0..max_x |> Enum.all?(fn x -> not MapSet.member?(space, {x, y}) end)
      end)
      |> MapSet.new()

    space
    |> Enum.map(fn {x, y} ->
      x_new = x + (factor - 1) * (Enum.take_while(columns, fn u -> u < x end) |> Enum.count())
      y_new = y + (factor - 1) * (Enum.take_while(rows, fn u -> u < y end) |> Enum.count())
      {x_new, y_new}
    end)
    |> MapSet.new()
  end

  @doc """
  iex> Dec11.shortest_path_lengths_sum(Dec11.parse("....#........
  ...>.........#...
  ...>#............
  ...>.............
  ...>.............
  ...>........#....
  ...>.#...........
  ...>............#
  ...>.............
  ...>.............
  ...>.........#...
  ...>#....#......."))
  374
  """
  def shortest_path_lengths_sum(space) do
    for {x, y} <- space, {u, v} <- space, x < u or (x == u and y < v) do
      abs(x - u) + abs(y - v)
    end
    |> Enum.sum()
  end

  @doc """
  iex> Dec11.solve("...#......
  ...>.......#..
  ...>#.........
  ...>..........
  ...>......#...
  ...>.#........
  ...>.........#
  ...>..........
  ...>.......#..
  ...>#...#.....", 2)
  374
  iex> Dec11.solve("...#......
  ...>.......#..
  ...>#.........
  ...>..........
  ...>......#...
  ...>.#........
  ...>.........#
  ...>..........
  ...>.......#..
  ...>#...#.....", 10)
  1030
  iex> Dec11.solve("...#......
  ...>.......#..
  ...>#.........
  ...>..........
  ...>......#...
  ...>.#........
  ...>.........#
  ...>..........
  ...>.......#..
  ...>#...#.....", 100)
  8410
  """
  def solve(input, factor) do
    input
    |> parse()
    |> expand(factor)
    |> shortest_path_lengths_sum()
  end

  @doc """
  iex> Dec11.a("...#......
  ...>.......#..
  ...>#.........
  ...>..........
  ...>......#...
  ...>.#........
  ...>.........#
  ...>..........
  ...>.......#..
  ...>#...#.....")
  374
  """
  def a(input) do
    input |> solve(2)
  end

  def b(input) do
    input |> solve(1_000_000)
  end
end
