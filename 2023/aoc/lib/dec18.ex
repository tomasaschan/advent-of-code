defmodule Dec18 do
  defmodule Instruction do
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
    ...>" |> Dec18.Instruction.parse_simple()
    [{:R, 6}, {:D, 5}, {:L, 2}, {:D, 2}, {:R, 2}, {:D, 2}, {:L, 5}, {:U, 2}, {:L, 1}, {:U, 2}, {:R, 2}, {:U, 3}, {:L, 2}, {:U, 2}]
    """
    def parse_simple(input) do
      input
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        %{"dir" => dir, "steps" => steps} =
          Regex.named_captures(
            ~r/(?<dir>[RLDU]) (?<steps>\d+)/,
            line
          )

        {
          String.to_atom(dir),
          String.to_integer(steps)
        }
      end)
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
    ...>" |> Dec18.Instruction.parse_complicated()
    [{:R, 461937},{:D, 56407},{:R, 356671},{:D, 863240},{:R, 367720},{:D, 266681},{:L, 577262},{:U, 829975},{:L, 112010},{:D, 829975},{:L, 491645},{:U, 686074},{:L, 5411},{:U, 500254}]
    """
    def parse_complicated(input) do
      input
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        %{"dir" => dir, "steps" => steps} =
          Regex.named_captures(~r/\(#(?<steps>[0-9a-f]{5})(?<dir>[0-3])\)/, line)

        {
          case dir do
            "0" -> :R
            "1" -> :D
            "2" -> :L
            "3" -> :U
          end,
          String.to_integer(steps, 16)
        }
      end)
    end
  end

  defmodule Measure do
    defp step(:R, steps, {x, y}), do: {x + steps, y}
    defp step(:L, steps, {x, y}), do: {x - steps, y}
    defp step(:U, steps, {x, y}), do: {x, y + steps}
    defp step(:D, steps, {x, y}), do: {x, y - steps}

    defp vertices(instructions), do: vertices(instructions, [{0, 0}])
    defp vertices([], vs), do: vs |> Enum.reverse()

    defp vertices([{dir, steps} | instructions], [p | vs]),
      do: vertices(instructions, [step(dir, steps, p), p | vs])

    defp translate_to_first_quadrant(vertices) do
      {xlo, ylo} =
        vertices |> Enum.reduce(fn {x, y}, {xlo, ylo} -> {min(x, xlo), min(y, ylo)} end)

      vertices
      |> Enum.map(fn {x, y} -> {x + abs(xlo), y + abs(ylo)} end)
    end

    defp edges(vertices), do: Enum.zip([nil | vertices], vertices) |> Enum.drop(1)

    defp area(edges),
      do:
        edges
        |> Enum.map(fn {{x1, y1}, {x2, y2}} ->
          (y1 + y2) * (x2 - x1) + abs(y2 - y1) + abs(x2 - x1)
        end)
        |> Enum.sum()
        |> abs()
        |> then(&div(&1, 2))
        |> then(&(&1 + 1))

    def dig_size(instructions) do
      instructions
      |> vertices()
      |> translate_to_first_quadrant()
      |> edges()
      |> area()
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
    input
    |> Instruction.parse_simple()
    |> Measure.dig_size()
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
  ...>" |> Dec18.b()
  952408144115
  """
  def b(input) do
    input
    |> Instruction.parse_complicated()
    |> Measure.dig_size()
  end
end
