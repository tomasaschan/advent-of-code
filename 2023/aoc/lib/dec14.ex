defmodule Dec14 do
  @doc """
  iex> Dec14.parse("#.#\\n.OO\\nO..\\n")
  {%{
    {0, 0} => "#",
    {2, 0} => "#",
    {1, 1} => "O",
    {2, 1} => "O",
    {0, 2} => "O"
  }, 2, 2}
  """
  def parse(input, opts \\ [ignore_bounds: false]) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn
        {".", _} -> nil
        {c, x} -> {{x, y}, c}
      end)
      |> Enum.reject(&is_nil/1)
    end)
    |> Enum.reduce(
      {%{}, 0, 0},
      fn {{x, y}, c}, {state, xhi, yhi} ->
        {Map.put(state, {x, y}, c), max(x, xhi), max(y, yhi)}
      end
    )
    |> then(fn {state, xhi, yhi} ->
      if opts[:ignore_bounds], do: state, else: {state, xhi, yhi}
    end)
  end

  def roll(state, bounds, dir) do
    {xhi, yhi} = bounds

    case dir do
      # rolling north; working south
      {0, 1} -> roll(state, {0, 0}, bounds, dir, 0)
      # rolling west, working east
      {1, 0} -> roll(state, {0, 0}, bounds, dir, 0)
      # rolling south, working north
      {0, -1} -> roll(state, {0, yhi}, bounds, dir, 0)
      # rolling east, working west
      {-1, 0} -> roll(state, {xhi, 0}, bounds, dir, 0)
    end
  end

  def roll(state, {x, y}, {xhi, yhi}, {dx, dy}, spaces)
      when 0 <= x and x <= xhi and 0 <= y and y <= yhi do
    case state[{x, y}] do
      nil ->
        roll(state, {x + dx, y + dy}, {xhi, yhi}, {dx, dy}, spaces + 1)

      "O" ->
        roll(
          state |> Map.delete({x, y}) |> Map.put({x - spaces * dx, y - spaces * dy}, "O"),
          {x + dx, y + dy},
          {xhi, yhi},
          {dx, dy},
          spaces
        )

      "#" ->
        roll(state, {x + dx, y + dy}, {xhi, yhi}, {dx, dy}, 0)
    end
  end

  # rolling north, exhausting a column; move to the next one
  def roll(state, {x, y}, {xhi, yhi}, {0, 1}, _) when y > yhi,
    do: roll(state, {x + 1, 0}, {xhi, yhi}, {0, 1}, 0)

  # rolling norht, exhausting all columns; done
  def roll(state, {x, _}, {xhi, _}, {0, 1}, _) when x > xhi, do: state

  # rolling east, exhausting a row; move to the next one
  def roll(state, {x, y}, {xhi, yhi}, {1, 0}, _) when x > xhi,
    do: roll(state, {0, y + 1}, {xhi, yhi}, {1, 0}, 0)

  # rolling east, exhausting all rows; done
  def roll(state, {_, y}, {_, yhi}, {1, 0}, _) when y > yhi, do: state

  # rolling south, exhausting a column; move to the next one
  def roll(state, {x, y}, {xhi, yhi}, {0, -1}, _) when y < 0,
    do: roll(state, {x + 1, yhi}, {xhi, yhi}, {0, -1}, 0)

  # rolling south, exhausting all columns; done
  def roll(state, {x, _}, {xhi, _}, {0, -1}, _) when x > xhi, do: state

  # rolling west, exhausting a row; move to the next one
  def roll(state, {x, y}, {xhi, yhi}, {-1, 0}, _) when x < 0,
    do: roll(state, {xhi, y + 1}, {xhi, yhi}, {-1, 0}, 0)

  # rolling west, exhausting all rows; done
  def roll(state, {_, y}, {_, yhi}, {-1, 0}, _) when y > yhi, do: state

  @doc ~S"""
  iex> Dec14.parse("O....#....
  ...>O.OO#....#
  ...>.....##...
  ...>OO.#O....O
  ...>.O.....O#.
  ...>O.#..O.#.#
  ...>..O..#O..O
  ...>.......O..
  ...>#....###..
  ...>#OO..#....
  ...>")
  ...> |> Dec14.cycle_once()
  Dec14.parse(".....#....\n....#...O#\n...OO##...\n.OO#......\n.....OOO#.\n.O#...O#.#\n....O#....\n......OOOO\n#...O###..\n#..OO#....", ignore_bounds: true)
  iex> Dec14.parse("O....#....
  ...>O.OO#....#
  ...>.....##...
  ...>OO.#O....O
  ...>.O.....O#.
  ...>O.#..O.#.#
  ...>..O..#O..O
  ...>.......O..
  ...>#....###..
  ...>#OO..#....
  ...>")
  ...> |> Dec14.cycle(n: 2)
  Dec14.parse(".....#....\n....#...O#\n.....##...\n..O#......\n.....OOO#.\n.O#...O#.#\n....O#...O\n.......OOO\n#..OO###..\n#.OOO#...O\n", ignore_bounds: true)
  iex> Dec14.parse("O....#....
  ...>O.OO#....#
  ...>.....##...
  ...>OO.#O....O
  ...>.O.....O#.
  ...>O.#..O.#.#
  ...>..O..#O..O
  ...>.......O..
  ...>#....###..
  ...>#OO..#....
  ...>")
  ...> |> Dec14.cycle(n: 3)
  Dec14.parse(".....#....\n....#...O#\n.....##...\n..O#......\n.....OOO#.\n.O#...O#.#\n....O#...O\n.......OOO\n#...O###.O\n#.OOO#...O", ignore_bounds: true)
  """
  def cycle_once({state, xhi, yhi}) do
    state
    |> roll({xhi, yhi}, {0, 1})
    |> roll({xhi, yhi}, {1, 0})
    |> roll({xhi, yhi}, {0, -1})
    |> roll({xhi, yhi}, {-1, 0})
  end


  def cycle(state, n: n) do
    cycle(state, n: n, seen: %{})
  end

  def cycle({state,_,_}, n: 0, seen: _), do: state

  def cycle({state, xhi, yhi}, n: n, seen: seen) do
    k = key(state, xhi, yhi)

    case Map.get(seen, k) do
      nil ->
        next = cycle_once({state, xhi, yhi})
        cycle({next, xhi, yhi}, n: n - 1, seen: Map.put(seen, k, n))

      prev ->
        # counting down, so prev is larger
        diff = (prev - n)
        cycle({state, xhi, yhi}, n: rem(n, diff) , seen: %{})
    end
  end

  @doc """
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
  ...>" |> Dec14.parse() |> then(fn {state, xhi, yhi} -> Dec14.key(state, xhi, yhi) end)
  "O....#....|O.OO#....#|.....##...|OO.#O....O|.O.....O#.|O.#..O.#.#|..O..#O..O|.......O..|#....###..|#OO..#...."
  """
  def key(state, xhi, yhi) do
    for y <- 0..yhi do
      for x <- 0..xhi do
        state[{x, y}] || "."
      end
      |> Enum.join("")
    end
    |> Enum.join("|")
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
    {initial, xhi, yhi} = input |> parse()

    initial
    |> roll({xhi, yhi}, {0, 1})
    |> load(yhi)
  end

  @doc """
  iex> "O....#....
  ...>O.OO#....#
  ...>.....##...
  ...>OO.#O....O
  ...>.O.....O#.
  ...>O.#..O.#.#
  ...>..O..#O..O
  ...>.......O..
  ...>#....###..
  ...>#OO..#...."
  ...> |> Dec14.b()
  64
  """
  def b(input) do
    {initial, xhi, yhi} = input |> parse()

    {initial, xhi, yhi}
    |> cycle(n: 1_000_000_000)
    |> load(yhi)
  end
end
