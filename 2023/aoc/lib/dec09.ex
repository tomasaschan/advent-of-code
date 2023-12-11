defmodule Dec09 do
  @doc """
  iex> Dec09.parse("0 3 6 9 12 15\\n1 3 6 10 15 21\\n10 13 16 21 30 45\\n")
  [[0, 3, 6, 9, 12, 15], [1, 3, 6, 10, 15, 21], [10, 13, 16, 21, 30, 45]]
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> line |> String.split(" ") |> Enum.map(&String.to_integer/1) end)
  end

  def extrapolate(history, {take, combine}) do
    diffs = history |> Enum.zip(history |> Enum.drop(1)) |> Enum.map(fn {a, b} -> b - a end)

    extreme = take.(history)

    cond do
      Enum.all?(diffs, fn n -> n == 0 end) -> extreme
      true -> combine.(extreme, extrapolate(diffs, {take, combine}))
    end
  end

  @doc """
  iex> Dec09.future([0, 3, 6, 9, 12, 15])
  18
  iex> Dec09.future([1, 3, 6, 10, 15, 21])
  28
  iex> Dec09.future([10, 13, 16, 21, 30, 45])
  68
  """
  def future(history) do
    extrapolate(history, {&List.last/1, &(&1 + &2)})
  end

  @doc """
  iex> Dec09.a("0 3 6 9 12 15\\n1 3 6 10 15 21\\n10 13 16 21 30 45\\n")
  114
  """
  def a(input) do
    histories = parse(input)

    histories
    |> Enum.map(&future/1)
    |> Enum.sum()
  end

  @doc """
  iex> Dec09.past([0, 3, 6, 9, 12, 15])
  -3
  iex> Dec09.past([1, 3, 6, 10, 15, 21])
  0
  iex> Dec09.past([10, 13, 16, 21, 30, 45])
  5
  """
  def past(history) do
    extrapolate(history, {&List.first/1, &(&1 - &2)})
  end

  @doc """
  iex> Dec09.b("0 3 6 9 12 15\\n1 3 6 10 15 21\\n10 13 16 21 30 45\\n")
  2
  """
  def b(input) do
    histories = parse(input)

    histories
    |> Enum.map(&past/1)
    |> Enum.sum()
  end
end
