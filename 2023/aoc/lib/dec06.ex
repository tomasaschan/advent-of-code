defmodule Dec06 do
  @doc """
  iex> Dec06.parse_a("Time:      7  15   30\\nDistance:  9  40  200\\n")
  [{7,9}, {15,40}, {30,200}]
  """
  def parse_a(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      Regex.scan(~r/(\d+)/, line, capture: :all_but_first)
      |> Enum.concat()
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.zip()
  end

  @doc """
  iex> Dec06.parse_b("Time:      7  15   30\\nDistance:  9  40  200\\n")
  {71530, 940200}
  """
  def parse_b(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn s ->
      s |> String.replace(" ", "") |> String.split(":") |> Enum.at(1) |> String.to_integer()
    end)
    |> List.to_tuple()
  end

  @doc """
  iex> Dec06.find_limits({7, 9})
  {2, 5}
  iex> Dec06.find_limits({15, 40})
  {4, 11}
  iex> Dec06.find_limits({30, 200})
  {11, 19}
  """
  def find_limits({time, distance}) do
    sq = (time ** 2 / 4 - distance) ** 0.5

    t_lo = time / 2 - sq
    t_hi = time / 2 + sq

    {
      if t_lo != round(t_lo) do
        ceil(t_lo)
      else
        round(t_lo) + 1
      end,
      if t_hi != round(t_hi) do
        floor(t_hi)
      else
        round(t_hi) - 1
      end
    }
  end

  def margins(race) do
    {lo, hi} = race
    hi - lo + 1
  end

  @doc """
  iex> Dec06.a("Time:      7  15   30\\nDistance:  9  40  200\\n")
  288
  """
  def a(input) do
    parse_a(input)
    |> Enum.map(&find_limits/1)
    |> Enum.map(&margins/1)
    |> Enum.product()
  end

  def b(input) do
    parse_b(input) |> then(&find_limits/1) |> then(&margins/1)
  end
end
