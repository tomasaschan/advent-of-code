defmodule Dec14 do
  @doc """
  iex> Dec14.parse("#.#\\n.OO\\nO..\\n")
  %{
    {0, 0} => "#",
    {2, 0} => "#",
    {1, 1} => "O",
    {2, 1} => "O",
    {0, 2} => "O"
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
      |> Enum.map(fn
        {".", _} -> nil
        {c, x} -> %{{x, y} => c}
      end)
      |> Enum.reject(&is_nil/1)
    end)
    |> Enum.reduce(&Map.merge/2)
  end

  @doc ~S"""
  before: #.#.
          .OO.
          O..O

  after:  #O#O
          O.O.
          ....

  iex> Dec14.roll(Dec14.parse("#.#.\n.OO.\nO..O\n"), 0, 0, 3, 2, 0)
  Dec14.parse("#O#O\nO.O.\n....")
  """
  def roll(state, x, y, xhi, yhi, spaces \\ 0)

  def roll(state, x, y, xhi, yhi, spaces) when y <= yhi do
    case state[{x, y}] do
      nil ->
        roll(state, x, y + 1, xhi, yhi, spaces + 1)

      "O" ->
        roll(
          state |> Map.delete({x, y}) |> Map.put({x, y - spaces}, "O"),
          x,
          y + 1,
          xhi,
          yhi,
          spaces
        )

      "#" ->
        roll(state, x, y + 1, xhi, yhi, 0)
    end
  end

  def roll(state, x, y, xhi, yhi, _) when y > yhi and x <= xhi do
    roll(state, x + 1, 0, xhi, yhi, 0)
  end

  def roll(state, x, _, xhi, _, _) when x > xhi, do: state

  def show(state, delimiter \\ "\n") do
    {xhi, yhi} = bounds(state)

    for y <- 0..yhi do
      for x <- 0..xhi do
        state[{x, y}] || "."
      end
      |> Enum.join("")
    end
    |> Enum.join(delimiter)
  end

  def bounds(state) do
    state
    |> Map.keys()
    |> Enum.reduce({0, 0}, fn {x, y}, {xhi, yhi} -> {max(x, xhi), max(y, yhi)} end)
  end

  def load(state, yhi) do
    state
    |> Enum.map(fn
      {{_, y}, "O"} -> yhi - y + 1
      _ -> 0
    end)
    |> Enum.sum()
  end

  @doc ~S"""
  iex> "O....#....
  ...>O.OO#....#
  ...>.....##...
  ...>OO.#O....O
  ...>.O.....O#.
  ...>O.#..O.#.#
  ...>..O..#O..O
  ...>.......O..
  ...>#....###..
  ...>#OO..#....
  ...>" |> Dec14.a()
  136
  """
  def a(input) do
    initial = input |> parse()

    {xhi, yhi} = bounds(initial)

    initial
    |> roll(0, 0, xhi, yhi, 0)
    |> load(yhi)
  end
end
