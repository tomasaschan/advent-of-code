defmodule Dec07 do
  @doc """
  iex> Dec07.parse("32T3K 765")
  [{{3, 2, :T, 3, :K}, 765}]
  iex> Dec07.parse("T55J5 684")
  [{{:T, 5, 5, :J, 5}, 684}]
  iex> Dec07.parse("KK677 28")
  [{{:K, :K, 6, 7, 7}, 28}]
  iex> Dec07.parse("KTJJT 220\\nQQQJA 483\\n")
  [{{:K, :T, :J, :J, :T}, 220}, {{:Q, :Q, :Q, :J, :A}, 483}]
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> String.split(line, " ", trim: true) end)
    |> Enum.map(fn [hand, bid] -> {cards(hand), String.to_integer(bid)} end)
  end

  def cards(hand) do
    hand
    |> String.graphemes()
    |> Enum.map(&card/1)
    |> List.to_tuple()
  end

  def card("A"), do: :A
  def card("K"), do: :K
  def card("Q"), do: :Q
  def card("J"), do: :J
  def card("T"), do: :T
  def card(i), do: String.to_integer(i)

  @doc """
  iex> Dec07.classify({3, 2, :T, 3, :K}, jack_wild: false)
  :one_pair
  iex> Dec07.classify({:T, 5, 5, :J, 5}, jack_wild: true)
  :four_of_a_kind
  """
  def classify(hand, jack_wild: false) do
    hand
    |> Tuple.to_list()
    |> Enum.group_by(&Function.identity/1)
    |> Enum.map(fn {rank, cards} -> {rank, Enum.count(cards)} end)
    |> Enum.sort_by(fn {_, count} -> count end, :desc)
    |> then(fn grouped ->
      case grouped do
        [{_, 5}] -> :five_of_a_kind
        [{_, 4}, {_, 1}] -> :four_of_a_kind
        [{_, 3}, {_, 2}] -> :full_house
        [{_, 3} | _] -> :three_of_a_kind
        [{_, 2}, {_, 2}, {_, 1}] -> :two_pairs
        [{_, 2} | _] -> :one_pair
        _ -> :high_card
      end
    end)
  end

  def classify(hand, jack_wild: true) do
    jacks = hand |> Tuple.to_list() |> Enum.filter(fn c -> c == :J end) |> Enum.count()

    hand
    |> Tuple.to_list()
    |> Enum.filter(fn card -> card != :J end)
    |> Enum.group_by(&Function.identity/1)
    |> Enum.map(fn {_, cards} -> Enum.count(cards) end)
    |> Enum.sort(:desc)
    |> then(fn counts ->
      case {counts, jacks} do
        {[], 5} -> :five_of_a_kind
        {[5], 0} -> :five_of_a_kind
        {[4], 1} -> :five_of_a_kind
        {[4, 1], 0} -> :four_of_a_kind
        {[3], 2} -> :five_of_a_kind
        {[3, 1], 1} -> :four_of_a_kind
        {[3, 2], 0} -> :full_house
        {[3, 1, 1], 0} -> :three_of_a_kind
        {[2], 3} -> :five_of_a_kind
        {[2, 1], 2} -> :four_of_a_kind
        {[2, 2], 1} -> :full_house
        {[2, 1, 1], 1} -> :three_of_a_kind
        {[2, 2, 1], 0} -> :two_pairs
        {[2, 1, 1, 1], 0} -> :one_pair
        {[1], 4} -> :five_of_a_kind
        {[1, 1], 3} -> :four_of_a_kind
        {[1, 1, 1], 2} -> :three_of_a_kind
        {[1, 1, 1, 1], 1} -> :one_pair
        {[1, 1, 1, 1, 1], 0} -> :high_card
      end
    end)
  end

  def rank({hand, _}, jack_wild: jack_wild),
    do:
      {rank(classify(hand, jack_wild: jack_wild)),
       hand
       |> Tuple.to_list()
       |> Enum.map(
         if jack_wild do
           fn
             :J -> 0
             card -> rank(card)
           end
         else
           fn card -> rank(card) end
         end
       )
       |> List.to_tuple()}

  @doc """
  iex> [:four_of_a_kind, :full_house, :three_of_a_kind, :two_pairs, :one_pair, :high_card] |> Enum.shuffle() |> Enum.sort_by(fn x -> Dec07.rank(x) end, :desc)
  [:four_of_a_kind, :full_house, :three_of_a_kind, :two_pairs, :one_pair, :high_card]
  """
  def rank(:five_of_a_kind), do: 7
  def rank(:four_of_a_kind), do: 6
  def rank(:full_house), do: 5
  def rank(:three_of_a_kind), do: 4
  def rank(:two_pairs), do: 3
  def rank(:one_pair), do: 2
  def rank(:high_card), do: 1
  def rank(:A), do: 14
  def rank(:K), do: 13
  def rank(:Q), do: 12
  def rank(:J), do: 11
  def rank(:T), do: 10
  def rank(i), do: i

  @doc """
  iex> Dec07.a("32T3K 765\\nT55J5 684\\nKK677 28\\nKTJJT 220\\nQQQJA 483\\n")
  6440
  """
  def a(input) do
    parse(input)
    |> Enum.sort_by(fn hand -> rank(hand, jack_wild: false) end)
    |> Enum.with_index(1)
    |> Enum.map(fn {{_, bid}, index} -> bid * index end)
    |> Enum.sum()
  end

  @doc """
  iex> Dec07.b("32T3K 765\\nT55J5 684\\nKK677 28\\nKTJJT 220\\nQQQJA 483\\n")
  5905
  """
  def b(input) do
    parse(input)
    |> Enum.sort_by(fn hand -> rank(hand, jack_wild: true) end)
    |> Enum.with_index(1)
    |> Enum.map(fn {{_, bid}, index} -> bid * index end)
    |> Enum.sum()
  end

  def show({{hand, bid}, index}, jack_wild: jack_wild),
    do: "#{show(hand)} (#{classify(hand, jack_wild: jack_wild)}) #{bid}*#{index} = #{bid * index}"

  def show({a, b, c, d, e}),
    do: "#{show(a)}#{show(b)}#{show(c)}#{show(d)}#{show(e)}"

  def show(:A), do: "A"
  def show(:K), do: "K"
  def show(:Q), do: "Q"
  def show(:J), do: "J"
  def show(:T), do: "T"
  def show(i), do: Integer.to_string(i)
end
