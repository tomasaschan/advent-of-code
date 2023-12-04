defmodule Dec02 do
  @doc """
  iex> Dec02.parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  {1, [%{blue: 3, red: 4}, %{red: 1, green: 2, blue: 6}, %{green: 2}]}
  """
  def parse_game(game_line) do
    id = Regex.run(~r/Game (\d+):/, game_line) |> Enum.at(1) |> String.to_integer()

    games =
      Regex.run(~r/Game \d+: (.*)/, game_line)
      |> Enum.at(1)
      |> String.split("; ")
      |> Enum.flat_map(fn input ->
        input
        |> String.split("; ")
        |> Enum.map(fn draft ->
          draft
          |> String.split(", ")
          |> Enum.map(fn draft ->
            Regex.run(~r/(\d+) (\w+)/, draft)
            |> Enum.drop(1)
            |> Kernel.then(fn [count, color] ->
              %{String.to_atom(color) => String.to_integer(count)}
            end)
            |> Map.new()
          end)
          |> Enum.reduce(&Map.merge/2)
        end)
      end)

    {id, games}
  end

  @doc """
  iex> Dec02.possible("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", %{:red=>12, :green=>13,:blue=>14})
  {1, true}
  iex> Dec02.possible("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", %{:red=>12, :green=>13,:blue=>14})
  {2, true}
  iex> Dec02.possible("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", %{:red=>12, :green=>13,:blue=>14})
  {3, false}
  iex> Dec02.possible("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", %{:red=>12, :green=>13,:blue=>14})
  {4, false}
  iex> Dec02.possible("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", %{:red=>12, :green=>13,:blue=>14})
  {5, true}
  """
  def possible(game, available) do
    {id, drafts} = parse_game(game)

    possible =
      Enum.all?(drafts, fn draft ->
        Enum.all?(draft, fn {color, count} -> Map.get(available, color, 0) >= count end)
      end)

    {id, possible}
  end

  @doc """
  iex> Dec02.a("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\\n")
  8
  """
  def a(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn game -> possible(game, %{red: 12, green: 13, blue: 14}) end)
    |> Enum.filter(fn {_, possible} -> possible end)
    |> Enum.map(fn {id, _} -> id end)
    |> Enum.sum()
  end

  @doc """
  iex> Dec02.minimize("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  %{red: 4, green: 2, blue: 6}
  iex> Dec02.minimize("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
  %{red: 1, green: 3, blue: 4}
  iex> Dec02.minimize("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
  %{red: 20, green: 13, blue: 6}
  iex> Dec02.minimize("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
  %{red: 14, green: 3, blue: 15}
  iex> Dec02.minimize("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
  %{red: 6, green: 3, blue: 2}
  """
  def minimize(game) do
    {_, drafts} = parse_game(game)

    drafts
    |> Enum.reduce(%{}, fn draft, acc -> Map.merge(acc, draft, fn _, a, b -> max(a, b) end) end)
  end

  @doc """
  iex> Dec02.power(%{red: 4, green: 2, blue: 6})
  48
  iex> Dec02.power(%{red: 1, green: 3, blue: 4})
  12
  iex> Dec02.power(%{red: 20, green: 13, blue: 6})
  1560
  iex> Dec02.power(%{red: 14, green: 3, blue: 15})
  630
  iex> Dec02.power(%{red: 6, green: 3, blue: 2})
  36
  """
  def power(game) do
    game |> Map.values() |> Enum.product()
  end

  @doc """
  iex> Dec02.b("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\\n")
  2286
  """
  def b(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&minimize/1)
    |> Enum.map(&power/1)
    |> Enum.sum()
  end
end
