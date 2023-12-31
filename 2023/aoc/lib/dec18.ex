defmodule Dec18 do
  defmodule Instruction do
    defstruct [:dir, :steps, :color]

    @doc """
    iex> "R 6 (#70c710)
    ...>D 5 (#0dc571)
    ...>L 2 (#5713f0)
    ...>D 2 (#d2c081)
    ...>R 2 (#59c680)
    ...>D 2 (#411b91)
    ...>L 5 (#8ceee2)
    ...>U 2 (#caa173)
    ...>L 1 (#1b58a2)
    ...>U 2 (#caa171)
    ...>R 2 (#7807d2)
    ...>U 3 (#a77fa3)
    ...>L 2 (#015232)
    ...>U 2 (#7a21e3)
    ...>" |> Dec18.Instruction.parse() |> Enum.count()
    14
    """
    def parse(input) do
      input
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        %{"dir" => dir, "steps" => steps, "r" => r, "g" => g, "b" => b} =
          Regex.named_captures(
            ~r/(?<dir>[RLDU]) (?<steps>\d+) \(#(?<r>[0-9a-f]{2})(?<g>[0-9a-f]{2})(?<b>[0-9a-f]{2})\)/,
            line
          )

        %Instruction{
          dir: String.to_atom(dir),
          steps: String.to_integer(steps),
          color: {String.to_integer(r, 16), String.to_integer(g, 16), String.to_integer(b, 16)}
        }
      end)
    end
  end

  defmodule Dig do
    def outline(instructions) do
      {grid, xlo, xhi, ylo, yhi, _} =
        instructions
        |> Enum.reduce({%{}, 0, 0, 0, 0, {0, 0}}, fn instruction, {grid, xlo, xhi, ylo, yhi, p} ->
          ditch(p, instruction, grid, xlo, xhi, ylo, yhi)
        end)

      {grid, xlo, xhi, ylo, yhi}
    end

    defp ditch(p, instr, grid, xlo, xhi, ylo, yhi) do
      ditch(p, instr.dir, instr.steps)
      |> Enum.reduce({grid, xlo, xhi, ylo, yhi, p}, fn {x, y}, {grid, xlo, xhi, ylo, yhi, _} ->
        grid = Map.put(grid, {x, y}, instr.color)
        xlo = min(xlo, x)
        xhi = max(xhi, x)
        ylo = min(ylo, y)
        yhi = max(yhi, y)
        {grid, xlo, xhi, ylo, yhi, {x, y}}
      end)
    end

    defp ditch({x, y}, :R, 0), do: [{x, y}]
    defp ditch({x, y}, :R, n), do: [{x, y} | ditch({x + 1, y}, :R, n - 1)]
    defp ditch({x, y}, :L, 0), do: [{x, y}]
    defp ditch({x, y}, :L, n), do: [{x, y} | ditch({x - 1, y}, :L, n - 1)]
    defp ditch({x, y}, :U, 0), do: [{x, y}]
    defp ditch({x, y}, :U, n), do: [{x, y} | ditch({x, y - 1}, :U, n - 1)]
    defp ditch({x, y}, :D, 0), do: [{x, y}]
    defp ditch({x, y}, :D, n), do: [{x, y} | ditch({x, y + 1}, :D, n - 1)]

    def dig({grid, xlo, xhi, ylo, yhi}),
      do: dig(grid, xlo, xhi, ylo, yhi, xlo, ylo + 1, {false, nil})

    defp wall_at(grid, x, y) do
      {Map.get(grid, {x, y - 1}, {255, 255, 255}) != {255, 255, 255}, Map.has_key?(grid, {x, y}),
       Map.get(grid, {x, y + 1}, {255, 255, 255}) != {255, 255, 255}}
    end

    def dig(grid, xlo, xhi, ylo, yhi, x, y, _) when x > xhi,
      do: dig(grid, xlo, xhi, ylo, yhi, xlo, y + 1, {false, nil})

    def dig(grid, xlo, xhi, ylo, yhi, _, y, _) when y >= yhi, do: {grid, xlo, xhi, ylo, yhi}

    def dig(grid, xlo, xhi, ylo, yhi, x, y, {true, nil}) do
      case wall_at(grid, x, y) do
        {_, false, _} ->
          dig(grid |> Map.put({x, y}, {255, 255, 255}), xlo, xhi, ylo, yhi, x + 1, y, {true, nil})

        {true, true, false} ->
          dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {true, :above})

        {false, true, true} ->
          dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {true, :below})

        {true, true, true} ->
          dig(grid, xlo, xhi, ylo, yhi, x + 2, y, {false, nil})
      end
    end

    def dig(grid, xlo, xhi, ylo, yhi, x, y, {false, nil}) do
      case wall_at(grid, x, y) do
        {_, false, _} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {false, nil})
        {true, true, false} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {false, :above})
        {false, true, true} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {false, :below})
        {true, true, true} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {true, nil})
      end
    end

    def dig(grid, xlo, xhi, ylo, yhi, x, y, {inside, :above}) do
      case wall_at(grid, x, y) do
        {false, true, false} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {inside, :above})
        {false, true, true} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {not inside, nil})
        {true, true, false} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {inside, nil})
      end
    end

    def dig(grid, xlo, xhi, ylo, yhi, x, y, {inside, :below}) do
      case wall_at(grid, x, y) do
        {false, true, false} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {inside, :below})
        {false, true, true} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {inside, nil})
        {true, true, false} -> dig(grid, xlo, xhi, ylo, yhi, x + 1, y, {not inside, nil})
      end
    end

    def show({grid, xlo, xhi, ylo, yhi}) do
      for y <- ylo..yhi do
        for x <- xlo..xhi do
          cond do
            Map.has_key?(grid, {x, y}) ->
              {r, g, b} = grid[{x, y}]
              IO.ANSI.color(trunc(r * 5 / 255), trunc(g * 5 / 255), trunc(b * 5 / 255)) <> "#"

            true ->
              IO.ANSI.light_black() <> "."
          end
        end
        |> Enum.join()
      end
      |> Enum.join("\n")
      |> then(fn s -> "\n\n" <> s <> IO.ANSI.reset() end)
    end
  end

  @doc """
  iex> "R 6 (#70c710)
  ...>D 5 (#0dc571)
  ...>L 2 (#5713f0)
  ...>D 2 (#d2c081)
  ...>R 2 (#59c680)
  ...>D 2 (#411b91)
  ...>L 5 (#8ceee2)
  ...>U 2 (#caa173)
  ...>L 1 (#1b58a2)
  ...>U 2 (#caa171)
  ...>R 2 (#7807d2)
  ...>U 3 (#a77fa3)
  ...>L 2 (#015232)
  ...>U 2 (#7a21e3)
  ...>" |> Dec18.a()
  62
  """
  def a(input) do
    {grid, _, _, _, _} =
      input
      |> Instruction.parse()
      |> Dig.outline()
      |> Dig.dig()

    map_size(grid)
  end
end
