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

  @doc """
  iex> "19, 13, 30 @ -2,  1, -2
  ...>18, 19, 22 @ -1, -1, -2
  ...>20, 25, 34 @ -2, -2, -4
  ...>12, 31, 28 @ -1, -2, -1
  ...>20, 19, 15 @  1, -5, -3
  ...>" |> Dec24.b()
  47
  """
  def b(input) do
    hailstorm = parse(input)

    # approach here is blatantly stolen from
    # * https://github.com/MarkSinke/aoc2023/blob/main/day24.go
    # * https://github.com/bjorng/advent-of-code-2023/blob/main/day24/lib/day24.ex

    # translate reference frame so that p0 is stationary; rock trajectory must now pass through origin
    [{p0, v0} | hailstorm] = hailstorm
    [{p1, v1} | hailstorm] = for({p, v} <- hailstorm, do: {sub(p, p0), sub(v, v0)})

    # the second hailstone follows a line somewhere in space; the plane that contains this line
    # and origo also contains the trajectory of the rock. the plane is described by the cross
    # product of two points vectors in it, which since origo is in it is just two points on the
    # trajectory of the hailstone, e.g. at t=0 and t=1.
    #
    # using the notation from https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
    # the plane is then described by the equation (p - p0) • n = 0; but p0 is origo, so we can simplify
    # to p • n = 0, where p is a point in the plane.
    n = cross(p1, add(p1, v1))

    # the intersection with other points is found also using https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
    [{t1, p1}, {t2, p2} | _] =
      hailstorm
      |> Enum.map(fn {l0, l} ->
        denom = dot(l, n)

        if denom == 0 do
          raise "degenerate case for hailstone {#{inspect(l0)}, #{inspect(l)}}"
        end

        t = div(dot(scale(l0, -1), n), denom)

        if t < 0 do
          raise "hailstone {#{inspect(l0)}, #{inspect(l)}} intersects in the past"
        end

        p = add(l0, scale(l, t))

        {t, p}
      end)

    dt = t2 - t1
    {dx, dy, dz} = sub(p2, p1)
    v = {div(dx, dt), div(dy, dt), div(dz, dt)}
    p = sub(p2, scale(v, t2))

    {x, y, z} = add(p, p0)
    x + y + z
  end

  def dot({x1, y1, z1}, {x2, y2, z2}) do
    x1 * x2 + y1 * y2 + z1 * z2
  end

  def cross({x1, y1, z1}, {x2, y2, z2}) do
    {
      y1 * z2 - z1 * y2,
      z1 * x2 - x1 * z2,
      x1 * y2 - y1 * x2
    }
  end

  def add({x1, y1, z1}, {x2, y2, z2}) do
    {x1 + x2, y1 + y2, z1 + z2}
  end

  def sub({x1, y1, z1}, {x2, y2, z2}) do
    {x1 - x2, y1 - y2, z1 - z2}
  end

  @doc """
  iex> Dec24.scale({1,2,3},4)
  {4, 8, 12}
  iex> Dec24.scale({1,2,3},-1)
  {-1, -2, -3}
  """
  def scale({x, y, z}, k) do
    {x * k, y * k, z * k}
  end

  def b_with_nr(hailstorm, lo, hi) do
    f = full_function(hailstorm)
    j = full_jacoubian(hailstorm)

    center = lo + (hi - lo) / 2

    guess =
      Enum.concat([
        [center, center, center, 0, 0, 0],
        hailstorm |> Enum.with_index() |> Enum.map(fn {_, i} -> i end)
      ])
      |> Nx.tensor()
      |> Nx.transpose()

    case full_newton_rhapson(f, j, guess) do
      :retry ->
        raise "did not converge"

      {:result, result} ->
        [x, y, z | _] = Nx.to_flat_list(result)
        round(x) + round(y) + round(z)
    end
  end

  def full_newton_rhapson(f, j, guess) do
    try do
      jac = j.(guess)
      IO.puts("inverting jacobian of shape #{inspect(jac.shape())}")
      jGenInv = Nx.LinAlg.pinv(j.(guess))

      next = Nx.subtract(guess, Nx.dot(jGenInv, f.(guess))) |> IO.inspect(label: "next")

      if Nx.to_number(Nx.LinAlg.norm(Nx.subtract(next, guess))) |> IO.inspect(label: "norm") <
           1 do
        {:result, guess}
      else
        full_newton_rhapson(f, j, next)
      end
    rescue
      ArithmeticError -> :retry
    end
  end

  def full_function(hailstorm) do
    fn guess ->
      [x, y, z, vx, vy, vz | ts] = Nx.to_flat_list(guess)

      hailstorm
      |> Enum.with_index()
      |> Enum.flat_map(fn {{{xi, yi, zi}, {vxi, vyi, vzi}}, i} ->
        ti = Enum.at(ts, i)

        [
          xi + ti * vxi - x - ti * vx,
          yi + ti * vyi - y - ti * vy,
          zi + ti * vzi - z - ti * vz
        ]
      end)
      |> Nx.tensor()
    end
  end

  def full_jacoubian(hailstorm) do
    fn guess ->
      [_, _, _, vx, vy, vz | ts] = Nx.to_flat_list(guess)

      hailstorm
      |> Enum.with_index()
      |> Enum.flat_map(fn {{_, {vxi, vyi, vzi}}, i} ->
        n = length(ts)
        ti = Enum.at(ts, i)

        prefix = Enum.map(0..(i - 1)//1, fn _ -> 0 end)
        postfix = Enum.map((i + 1)..(n - 1)//1, fn _ -> 0 end)

        [
          Enum.concat([[-1, 0, 0, -ti, 0, 0], prefix, [vxi - vx], postfix]),
          Enum.concat([[0, -1, 0, 0, -ti, 0], prefix, [vyi - vy], postfix]),
          Enum.concat([[0, 0, -1, 0, 0, -ti], prefix, [vzi - vz], postfix])
        ]
      end)
      |> Nx.tensor()
    end
  end
end
