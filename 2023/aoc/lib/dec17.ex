defmodule Dec17 do
  import PriorityQueue, only: [new: 0, put: 3, pop!: 1, size: 1]

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {c, x} -> {{x, y}, String.to_integer(c)} end)
    end)
    |> Enum.reduce({%{}, 0, 0}, fn {{x, y}, cost}, {grid, xhi, yhi} ->
      {grid |> Map.put({x, y}, cost), max(xhi, x), max(yhi, y)}
    end)
  end

  defmodule Step do
    def step(x, y, :n), do: {x, y - 1, :n}
    def step(x, y, :s), do: {x, y + 1, :s}
    def step(x, y, :e), do: {x + 1, y, :e}
    def step(x, y, :w), do: {x - 1, y, :w}

    defp opposite(:n), do: :s
    defp opposite(:s), do: :n
    defp opposite(:e), do: :w
    defp opposite(:w), do: :e

    defp all_turns(d), do: [:n, :s, :e, :w] |> Enum.reject(&(&1 == opposite(d)))

    def next_steps(d, s) do
      all_turns(d)
      |> Enum.map(fn
        ^d -> {d, s + 1}
        d -> {d, 1}
      end)
    end
  end

  defmodule Djikstra do
    def nexts(next_dirs) do
      fn {x, y, d, s, cost}, {grid, xhi, yhi} ->
        next_dirs.(d, s)
        |> Enum.map(fn {d, s} -> {Step.step(x, y, d), s} end)
        |> Enum.filter(fn {{x, y, _}, _} ->
          0 <= x and x <= xhi and 0 <= y and y <= yhi
        end)
        |> Enum.map(fn {{x, y, d}, s} -> {x, y, d, s, cost + grid[{x, y}]} end)
      end
    end

    def find_path({grid, xhi, yhi}, next_dirs, done, first_states) do
      q = first_states |> Enum.reduce(new(), fn s, q -> put(q, 0, {s, %{}}) end)
      Djikstra.find_path({q, MapSet.new(first_states)}, {grid, xhi, yhi}, nexts(next_dirs), done)
    end

    def find_path({q, seen}, {grid, xhi, yhi}, nexts, done) do
      if size(q) > 100 * xhi * yhi do
        raise "too many steps queued!"
      end

      {{cost, {{x, y, d, s}, path}}, qnxt} = pop!(q)

      cond do
        done.(x,y,s,xhi,yhi) ->          {cost, path}

        true ->
          nexts.({x, y, d, s, cost}, {grid, xhi, yhi})
          |> Enum.filter(fn {x, y, d, s, _} -> not MapSet.member?(seen, {x, y, d, s}) end)
          |> Enum.reduce({qnxt, seen}, fn {x, y, d, s, c}, {q, sn} ->
            {put(q, c, {{x, y, d, s}, Map.put(path, {x, y}, d)}), MapSet.put(sn, {x, y, d, s})}
          end)
          |> find_path({grid, xhi, yhi}, nexts, done)
      end
    end
  end

  defmodule Crucible do
    def done(x,y,_,xhi,yhi), do: x==xhi and y==yhi
    def next_dirs(d, s), do: Step.next_steps(d, s) |> Enum.filter(fn {_, s} -> s <= 3 end)
  end

  defmodule UltraCrucible do
    def done(x,y,s,xhi,yhi), do: x==xhi and y==yhi and s >= 4
    def next_dirs(d, s) when s < 4,
      do: Step.next_steps(d, s) |> Enum.filter(fn {nd, _} -> nd == d end)

    def next_dirs(d, s) when s >= 10,
      do: Step.next_steps(d, s) |> Enum.filter(fn {nd, _} -> nd != d end)

    def next_dirs(d, s), do: Step.next_steps(d, s)
  end

  @doc """
  iex> "2413432311323
  ...>3215453535623
  ...>3255245654254
  ...>3446585845452
  ...>4546657867536
  ...>1438598798454
  ...>4457876987766
  ...>3637877979653
  ...>4654967986887
  ...>4564679986453
  ...>1224686865563
  ...>2546548887735
  ...>4322674655533
  ...>" |> Dec17.a()
  102
  """
  def a(input) do
    {grid, xhi, yhi} = input |> parse()
    {cost, _} = Djikstra.find_path({grid, xhi, yhi}, &Crucible.next_dirs/2, &Crucible.done/5, [{0, 0, :e, 0}])

    cost
  end

  @doc """
  iex> "2413432311323
  ...>3215453535623
  ...>3255245654254
  ...>3446585845452
  ...>4546657867536
  ...>1438598798454
  ...>4457876987766
  ...>3637877979653
  ...>4654967986887
  ...>4564679986453
  ...>1224686865563
  ...>2546548887735
  ...>4322674655533
  ...>" |> Dec17.b()
  94

  iex> "111111111111
  ...>999999999991
  ...>999999999991
  ...>999999999991
  ...>999999999991
  ...>" |> Dec17.b()
  71
  """
  def b(input) do
    {grid, xhi, yhi} = input |> parse()
    {cost, _} =       Djikstra.find_path({grid, xhi, yhi}, &UltraCrucible.next_dirs/2, &UltraCrucible.done/5, [{0, 0, :e, 0},{0, 0, :s, 0}])

    cost
  end

  def show(grid, xhi, yhi, path, cost) do
    for y <- 0..yhi do
      for x <- 0..xhi do
        case Map.get(path, {x, y}) do
          :n -> "^"
          :s -> "v"
          :e -> ">"
          :w -> "<"
          _ -> Integer.to_string(grid[{x, y}])
        end
      end
      |> Enum.join()
    end
    |> Enum.join("\n")
    |> then(fn s -> "\n#{s} at cost #{cost}" end)
    |> IO.puts()
  end
end
