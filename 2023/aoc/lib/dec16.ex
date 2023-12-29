defmodule Dec16 do
  defmodule Mirrors do
    defstruct mirrors: %{}, xhi: 0, yhi: 0

    def parse(input) do
      input
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {line, y} ->
        line
        |> String.split("", trim: true)
        |> Enum.with_index()
        |> Enum.map(fn {char, x} ->
          {{x, y}, char}
        end)
      end)
      |> Enum.reduce(%Mirrors{}, fn {{x, y}, char}, mirrors ->
        %{
          mirrors
          | mirrors: Map.put(mirrors.mirrors, {x, y}, char),
            xhi: max(x, mirrors.xhi),
            yhi: max(y, mirrors.yhi)
        }
      end)
    end

    def show(%Mirrors{mirrors: map, xhi: xhi, yhi: yhi}) do
      0..yhi
      |> Enum.map(fn y -> 0..xhi |> Enum.map(fn x -> Map.get(map, {x, y}) end) |> Enum.join() end)
      |> Enum.join("\n")
    end
  end

  defmodule Rays do
    defstruct rays: MapSet.new(), xhi: 0, yhi: 0

    defp step(:up, {x, y}), do: {x, y - 1}
    defp step(:down, {x, y}), do: {x, y + 1}
    defp step(:left, {x, y}), do: {x - 1, y}
    defp step(:right, {x, y}), do: {x + 1, y}

    defp stepd(x, y, d), do: step(d, {x, y}) |> then(fn {x, y} -> {x, y, d} end)

    defp trace(mirrors, seen, q) do
      case :queue.out(q) do
        {:empty, _} ->
          seen

        {{:value, {x, y, d}}, qnxt} ->
          case {Map.get(mirrors.mirrors, {x, y}), d} do
            {nil, _} -> []
            {".", d} -> [stepd(x, y, d)]
            {"|", :up} -> [stepd(x, y, :up)]
            {"|", :down} -> [stepd(x, y, d)]
            {"|", :left} -> [stepd(x, y, :up), stepd(x, y, :down)]
            {"|", :right} -> [stepd(x, y, :up), stepd(x, y, :down)]
            {"-", :left} -> [stepd(x, y, :left)]
            {"-", :right} -> [stepd(x, y, :right)]
            {"-", :up} -> [stepd(x, y, :left), stepd(x, y, :right)]
            {"-", :down} -> [stepd(x, y, :left), stepd(x, y, :right)]
            {"/", :right} -> [stepd(x, y, :up)]
            {"/", :up} -> [stepd(x, y, :right)]
            {"/", :down} -> [stepd(x, y, :left)]
            {"/", :left} -> [stepd(x, y, :down)]
            {"\\", :up} -> [stepd(x, y, :left)]
            {"\\", :down} -> [stepd(x, y, :right)]
            {"\\", :left} -> [stepd(x, y, :up)]
            {"\\", :right} -> [stepd(x, y, :down)]
          end
          |> Enum.reject(fn p -> MapSet.member?(seen, p) end)
          |> Enum.reject(fn {x, y, _} -> x < 0 or y < 0 or x > mirrors.xhi or y > mirrors.yhi end)
          |> Enum.reduce({seen, qnxt}, fn p, {seen, q} ->
            {MapSet.put(seen, p), :queue.in(p, q)}
          end)
          |> then(fn {seen, q} -> trace(mirrors, seen, q) end)
      end
    end

    def trace(mirrors, from \\ {0, 0, :right}) do
      %Rays{
        rays:
          trace(
            mirrors,
            MapSet.put(MapSet.new(), from),
            :queue.in(from, :queue.new())
          ),
        xhi: mirrors.xhi,
        yhi: mirrors.yhi
      }
    end

    def show(rays, mirrors) do
      rays
      |> Enum.reduce(mirrors, fn {x, y, d}, mirrors ->
        %{
          mirrors
          | mirrors:
              Map.put(
                mirrors.mirrors,
                {x, y},
                case d do
                  :up -> "^"
                  :down -> "v"
                  :left -> "<"
                  :right -> ">"
                end
              )
        }
      end)
      |> Mirrors.show()
    end
  end

  defmodule Energize do
    defstruct(energized: MapSet.new(), xhi: 0, yhi: 0)

    def energized(rays) do
      %Energize{
        energized:
          rays.rays
          |> MapSet.to_list()
          |> Enum.map(fn {x, y, _} -> {x, y} end)
          |> Enum.sort()
          |> Enum.uniq()
          |> MapSet.new(),
        xhi: rays.xhi,
        yhi: rays.yhi
      }
    end

    def measure(energized) do
      energized.energized |> MapSet.size()
    end

    def show(energized) do
      0..energized.yhi
      |> Enum.map(fn y ->
        0..energized.xhi
        |> Enum.map(fn x ->
          if MapSet.member?(energized.energized, {x, y}) do
            "#"
          else
            "."
          end
        end)
        |> Enum.join()
      end)
      |> Enum.join("\n")
    end
  end

  @doc ~S"""
  iex> ".|...\\....
  ...>|.-.\\.....
  ...>.....|-...
  ...>........|.
  ...>..........
  ...>.........\\
  ...>..../.\\\\..
  ...>.-.-/..|..
  ...>.|....-|.\\
  ...>..//.|....
  ...>" |> Dec16.a()
  46
  """
  def a(input) do
    input
    |> Mirrors.parse()
    |> Rays.trace()
    |> Energize.energized()
    |> Energize.measure()
  end

  @doc ~S"""
  iex> ".|...\\....
  ...>|.-.\\.....
  ...>.....|-...
  ...>........|.
  ...>..........
  ...>.........\\
  ...>..../.\\\\..
  ...>.-.-/..|..
  ...>.|....-|.\\
  ...>..//.|....
  ...>" |> Dec16.b()
  51
  """
  def b(input) do
    input
    |> Mirrors.parse()
    |> optimize()
  end

  def optimize(mirrors) do
    [
      0..mirrors.xhi |> Enum.map(fn x -> {x, 0, :down} end),
      0..mirrors.yhi |> Enum.map(fn y -> {0, y, :right} end),
      0..mirrors.xhi |> Enum.map(fn x -> {x, mirrors.yhi, :up} end),
      0..mirrors.yhi |> Enum.map(fn y -> {mirrors.xhi, y, :left} end)
    ]
    |> Enum.concat()
    |> Enum.map(fn p -> Rays.trace(mirrors, p) |> Energize.energized() |> Energize.measure() end)
    |> Enum.max()
  end
end
