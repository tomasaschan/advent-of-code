defmodule Dec24 do
  def parse(input) do
    parse_3d = fn part ->
      part
      |> String.split(", ", trim: true)
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end

    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [p, v] = line |> String.split(" @ ", trim: true)

      position = parse_3d.(p)
      velocity = parse_3d.(v)

      {position, velocity}
    end)
  end

  def drop_z(hailstorm) do
    hailstorm
    |> Enum.map(fn {{x, y, _}, {vx, vy, _}} -> {{x, y}, {vx, vy}} end)
  end

  def intersection_2d({{x1, y1}, {vx1, vy1}}, {{x3, y3}, {vx3, vy3}}) do
    {x2, y2} = {x1 + vx1, y1 + vy1}
    {x4, y4} = {x3 + vx3, y3 + vy3}

    denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

    if denom == 0 do
      nil
    else
      t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
      u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denom

      if t < 0 or u < 0 do
        nil
      else
        px = x1 + t * (x2 - x1)
        py = y1 + t * (y2 - y1)
        {px, py}
      end
    end
  end

  def crossings_2d(hailstorm, lo, hi) do
    hailstorm = drop_z(hailstorm)

    for a <- hailstorm, b <- hailstorm, a < b do
      case intersection_2d(a, b) do
        {px, py} when lo <= px and px <= hi and lo <= py and py <= hi -> {px, py}
        _ -> nil
      end
    end
    |> Enum.reject(&is_nil/1)
  end

  @doc """
  iex> "19, 13, 30 @ -2,  1, -2
  ...>18, 19, 22 @ -1, -1, -2
  ...>20, 25, 34 @ -2, -2, -4
  ...>12, 31, 28 @ -1, -2, -1
  ...>20, 19, 15 @  1, -5, -3
  ...>" |> Dec24.a(7, 27)
  2
  """
  def a(input, lo \\ 200_000_000_000_000, hi \\ 400_000_000_000_000) do
    parse(input) |> crossings_2d(lo, hi) |> Enum.count()
  end
end
