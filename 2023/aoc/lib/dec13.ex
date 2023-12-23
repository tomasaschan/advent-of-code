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
  [{4, :vertical}]
  """
  def find_vertical_mirror(xhi, yhi, pattern, x) when x < xhi do
    if vertical_mirror?(xhi, yhi, x, pattern, 0) do
      {x, :vertical}
    else
      find_vertical_mirror(xhi, yhi, pattern, x + 1)
    end
  end

  def find_vertical_mirror(_, _, _, _), do: nil

  def find_vertical_mirrors(xhi, _, _, x0) when x0 > xhi, do: []

  def find_vertical_mirrors(xhi, yhi, pattern, x0) do
    case find_vertical_mirror(xhi, yhi, pattern, x0) do
      nil -> []
      {x, :vertical} -> [{x, :vertical} | find_vertical_mirrors(xhi, yhi, pattern, x + 1)]
    end
  end

  def find_horizontal_mirrors(_, yhi, _, y0) when y0 > yhi, do: []

  def find_horizontal_mirrors(xhi, yhi, pattern, y0) do
    case find_horizontal_mirror(xhi, yhi, pattern, y0) do
      nil -> []
      {y, :horizontal} -> [{y, :horizontal} | find_horizontal_mirrors(xhi, yhi, pattern, y + 1)]
    end
  end

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
  [{3, :horizontal}, {3, :horizontal}]
  """
  def find_horizontal_mirror(xhi, yhi, pattern, y) when y < yhi do
    if horizontal_mirror?(xhi, yhi, y, pattern, 0) do
      {y, :horizontal}
    else
      find_horizontal_mirror(xhi, yhi, pattern, y + 1)
    end
  end

  def find_horizontal_mirror(_, _, _, _), do: nil

  def find_mirrors(xhi, yhi, pattern) do
    Enum.concat(
      find_vertical_mirrors(xhi, yhi, pattern, 0),
      find_horizontal_mirrors(xhi, yhi, pattern, 0)
    )
    |> Enum.reject(&is_nil/1)
  end

  def summarize(mirrors) do
    mirrors
    |> Enum.reject(&is_nil/1)
    |> Enum.map(fn
      {v, :vertical} -> v + 1
      {h, :horizontal} -> 100 * (h + 1)
    end)
    |> Enum.sum()
  end

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
    |> Enum.flat_map(fn {xhi, yhi, pattern} -> find_mirrors(xhi, yhi, pattern) end)
    |> summarize()
  end

  @doc """
  iex> Dec13.parse(".#..#......
  ...>..#.#......
  ...>..#...#....
  ...>#.##...####
  ...>.#..#..####
  ...>#.#.##.####
  ...>###..#.#..#
  ...>")
  ...>|> Enum.all?(fn {xhi, yhi, pattern} -> length(Dec13.variants(xhi, yhi, pattern, 0, 0)) == (xhi+1)*(yhi+1) end)
  true
  iex> Dec13.parse("...\\n...\\n...")
  ...> |> Enum.map(fn {xhi, yhi, pattern} -> Dec13.variants(xhi, yhi, pattern, 0, 0) |> Enum.map(fn variant -> Dec13.show(xhi, yhi, variant) end) end)
  ...> |> Enum.flat_map(&Function.identity/1)
  ...> |> Enum.join("\\n=====\\n")
  "  012
  0 #..
  1 ...
  2 ...
  =====
    012
  0 .#.
  1 ...
  2 ...
  =====
    012
  0 ..#
  1 ...
  2 ...
  =====
    012
  0 ...
  1 #..
  2 ...
  =====
    012
  0 ...
  1 .#.
  2 ...
  =====
    012
  0 ...
  1 ..#
  2 ...
  =====
    012
  0 ...
  1 ...
  2 #..
  =====
    012
  0 ...
  1 ...
  2 .#.
  =====
    012
  0 ...
  1 ...
  2 ..#"
  """
  def variants(xhi, yhi, pattern, x, y) when x < xhi do
    next =
      case pattern[{x, y}] do
        "." -> "#"
        "#" -> "."
      end

    [Map.put(pattern, {x, y}, next) | variants(xhi, yhi, pattern, x + 1, y)]
  end

  def variants(xhi, yhi, pattern, x, y) when x == xhi and y < yhi do
    next =
      case pattern[{x, y}] do
        "." -> "#"
        "#" -> "."
      end

    [Map.put(pattern, {x, y}, next) | variants(xhi, yhi, pattern, 0, y + 1)]
  end

  def variants(xhi, yhi, pattern, x, y) when x == xhi and y == yhi do
    next =
      case pattern[{x, y}] do
        "." -> "#"
        "#" -> "."
      end

    [Map.put(pattern, {x, y}, next)]
  end

  @doc """
  iex> Dec13.b("#.##..##.
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
  400
  """
  def b(input) do
    parse(input)
    |> Enum.flat_map(&b1/1)
    |> summarize()
  end

  def b1({xhi, yhi, pattern}) do
    original = find_mirrors(xhi, yhi, pattern)

    variants(xhi, yhi, pattern, 0, 0)
    |> Enum.map(fn variant ->
      find_mirrors(xhi, yhi, variant)
      |> Enum.reject(&Enum.member?(original, &1))
    end)
    |> Enum.flat_map(&Function.identity/1)
    |> Enum.uniq()
  end

  def show(xhi, yhi, pattern) do
    top_row = 0..xhi |> Enum.map(fn x -> rem(x, 10) end) |> Enum.join("")

    y_digits = (ElixirMath.log10(1.0 * yhi) |> trunc()) + 1

    rows =
      0..yhi
      |> Enum.map(fn y ->
        prefix = "#{String.pad_leading(Integer.to_string(y), y_digits, " ")}"
        line = 0..xhi |> Enum.map(fn x -> pattern[{x, y}] end) |> Enum.join("")
        prefix <> " " <> line
      end)
      |> Enum.join("\n")

    "#{String.duplicate(" ", y_digits + 1)}#{top_row}\n" <> rows
  end
end
