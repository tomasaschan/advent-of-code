defmodule Dec13 do
  def parse(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(fn pattern ->
      pattern
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {line, y} ->
        line
        |> String.split("", trim: true)
        |> Enum.with_index()
        |> Enum.map(fn {c, x} -> {{x, y}, c} end)
        |> Enum.reduce({0, 0, %{}}, fn {{x, y}, c}, {xhi, yhi, pattern} ->
          {max(x, xhi), max(y, yhi), Map.put(pattern, {x, y}, c)}
        end)
      end)
      |> Enum.reduce({0, 0, %{}}, fn {x, y, m}, {xhi, yhi, pattern} ->
        {max(x, xhi), max(y, yhi), Map.merge(m, pattern)}
      end)
    end)
  end

  def vertical_reflection?(0, pattern, x1, x2) do
    pattern[{x1, 0}] == pattern[{x2, 0}]
  end

  def vertical_reflection?(y, pattern, x1, x2) do
    pattern[{x1, y}] == pattern[{x2, y}] && vertical_reflection?(y - 1, pattern, x1, x2)
  end

  def horizontal_reflection?(0, pattern, y1, y2) do
    pattern[{0, y1}] == pattern[{0, y2}]
  end

  def horizontal_reflection?(x, pattern, y1, y2) do
    pattern[{x, y1}] == pattern[{x, y2}] && horizontal_reflection?(x - 1, pattern, y1, y2)
  end

  def vertical_mirror?(xhi, _, x, _, n) when x - n < 0 or x + n + 1 > xhi, do: true

  def vertical_mirror?(xhi, yhi, x, pattern, n) do
    if not vertical_reflection?(yhi, pattern, x - n, x + n + 1) do
      false
    else
      vertical_mirror?(xhi, yhi, x, pattern, n + 1)
    end
  end

  def horizontal_mirror?(_, yhi, y, _, n) when y - n < 0 or y + n + 1 > yhi, do: true

  def horizontal_mirror?(xhi, yhi, y, pattern, n) do
    if not horizontal_reflection?(xhi, pattern, y - n, y + n + 1) do
      false
    else
      horizontal_mirror?(xhi, yhi, y, pattern, n + 1)
    end
  end

  @doc """
  iex> Dec13.parse("#.##..##.
  ...>..#.##.#.
  ...>##......#
  ...>##......#
  ...>..#.##.#.
  ...>..##..##.
  ...>#.#.##.#.
  ...>") |> Enum.map(fn {xhi, yhi, pattern} -> Dec13.find_vertical_mirror(xhi, yhi, pattern, 0) end)
  [4]
  """
  def find_vertical_mirror(xhi, yhi, pattern, x) when x < xhi do
    if vertical_mirror?(xhi, yhi, x, pattern, 0) do
      x
    else
      find_vertical_mirror(xhi, yhi, pattern, x + 1)
    end
  end

  def find_vertical_mirror(_, _, _, _), do: nil

  @doc """
  iex> Dec13.parse("#...##..#
  ...>#....#..#
  ...>..##..###
  ...>#####.##.
  ...>#####.##.
  ...>..##..###
  ...>#....#..#
  ...>
  ...>.#.##.#.###
  ...>..####..##.
  ...>#########..
  ...>.##..##..##
  ...>.##..##..##
  ...>#########..
  ...>..####..##.
  ...>.#.##.#.###
  ...>#.#..#.##.#
  ...>..#####.###
  ...>...##.....#
  ...>..#..#..#..
  ...>...##...#..
  ...>") |> Enum.map(fn {xhi,yhi,pattern} -> Dec13.find_horizontal_mirror(xhi, yhi, pattern, 0) end)
  [3, 3]
  """
  def find_horizontal_mirror(xhi, yhi, pattern, y) when y < yhi do
    if horizontal_mirror?(xhi, yhi, y, pattern, 0) do
      y
    else
      find_horizontal_mirror(xhi, yhi, pattern, y + 1)
    end
  end

  def find_horizontal_mirror(_, _, _, _), do: nil

  @doc """
  iex> Dec13.a("#.##..##.
  ...>..#.##.#.
  ...>##......#
  ...>##......#
  ...>..#.##.#.
  ...>..##..##.
  ...>#.#.##.#.
  ...>
  ...>#...##..#
  ...>#....#..#
  ...>..##..###
  ...>#####.##.
  ...>#####.##.
  ...>..##..###
  ...>#....#..#
  ...>")
  405
  """
  def a(input) do
    parse(input)
    |> Enum.with_index()
    |> Enum.map(fn {{xhi, yhi, pattern}, i} ->
      {v, h} =
        {find_vertical_mirror(xhi, yhi, pattern, 0), find_horizontal_mirror(xhi, yhi, pattern, 0)}

      if v == nil and h == nil do
        IO.puts("No mirror found for pattern #{i}")
      end

      {v, h}
    end)
    |> Enum.map(fn
      {x, nil} -> x + 1
      {nil, y} -> 100 * (y + 1)
    end)
    |> Enum.sum()
  end
end
