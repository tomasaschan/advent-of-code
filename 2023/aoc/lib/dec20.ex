defmodule Dec20 do
  @doc """
  iex> "broadcaster -> a, b, c
  ...>%a -> b
  ...>%b -> c
  ...>%c -> inv
  ...>&inv -> a
  ...>" |> Dec20.parse()
  %{
    broadcaster: %{type: :plain, in: [], out: [:a, :b, :c]},
    a: %{type: :flip_flop, in: [:broadcaster, :inv], out: [:b]},
    b: %{type: :flip_flop, in: [:a, :broadcaster], out: [:c]},
    c: %{type: :flip_flop, in: [:b, :broadcaster], out: [:inv]},
    inv: %{type: :conjunction, in: [:c], out: [:a]},
  }
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn
      "%" <> rest ->
        [name, nexts] = rest |> String.split(" -> ", trim: true)
        nexts = nexts |> String.split(", ", trim: true) |> Enum.map(&String.to_atom/1)
        {:flip_flop, String.to_atom(name), nexts}

      "&" <> rest ->
        [name, nexts] = rest |> String.split(" -> ", trim: true)
        nexts = nexts |> String.split(", ", trim: true) |> Enum.map(&String.to_atom/1)
        {:conjunction, String.to_atom(name), nexts}

      line ->
        [name, nexts] = line |> String.split(" -> ", trim: true)
        nexts = nexts |> String.split(", ", trim: true) |> Enum.map(&String.to_atom/1)
        {:plain, String.to_atom(name), nexts}
    end)
    |> Enum.reduce(%{}, fn {type, name, nexts}, modules ->
      inputs =
        nexts
        |> Enum.map(fn next -> %{next => %{in: [name]}} end)
        |> Enum.reduce(
          Map.new()
          |> Map.put(name, %{
            type: type,
            out: nexts,
            in: []
          }),
          &Map.merge/2
        )

      Map.merge(modules, inputs, fn _, a, b ->
        Map.merge(a, b, fn
          :in, a, b -> (a ++ b) |> Enum.sort()
          :out, a, b -> (a ++ b) |> Enum.sort()
        end)
      end)
    end)
  end

  @doc """
  iex> %{
  ...>   broadcaster: %{type: :plain, in: [], out: [:a, :b, :c]},
  ...>   a: %{type: :flip_flop, in: [:broadcaster, :inv], out: [:b]},
  ...>   b: %{type: :flip_flop, in: [:a, :broadcaster], out: [:c]},
  ...>   c: %{type: :flip_flop, in: [:b, :broadcaster], out: [:inv]},
  ...>   inv: %{type: :conjunction, in: [:c], out: [:a]},
  ...> } |> Dec20.initial_state()
  %{
    a: false,
    b: false,
    c: false,
    inv: %{c: false},
  }
  """
  def initial_state(modules) do
    modules
    |> Enum.map(fn
      {name, %{type: :flip_flop}} ->
        {name, false}

      {name, %{type: :conjunction}} ->
        {name, modules[name][:in] |> Enum.map(fn input -> {input, false} end) |> Map.new()}

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
    |> Map.new()
  end

  def press_button(_, _, trackers, 0), do: trackers

  def press_button(modules, state, trackers, n) do
    {state, trackers} = press_button(modules, state, trackers)
    press_button(modules, state, trackers, n - 1)
  end

  @doc """
  iex> %{
  ...>   broadcaster: %{type: :plain, in: [], out: [:a, :b, :c]},
  ...>   a: %{type: :flip_flop, in: [:broadcaster, :inv], out: [:b]},
  ...>   b: %{type: :flip_flop, in: [:a, :broadcaster], out: [:c]},
  ...>   c: %{type: :flip_flop, in: [:b, :broadcaster], out: [:inv]},
  ...>   inv: %{type: :conjunction, in: [:c], out: [:a]},
  ...> } |> Dec20.press_button(%{a: false, b: false, c: false, inv: %{c: false}}, %{:button => 0, true => 0, false => 0})
  {
    %{a: false, b: false, c: false, inv: %{c: false}},
    %{button: 1, true: 4, false: 8}
  }
  """
  def press_button(modules, state, trackers) do
    send_pulses(
      modules,
      state,
      track_button_press(trackers),
      :queue.in({:broadcaster, :plain, false, :button}, :queue.new())
    )
  end

  defp send_pulses(modules, state, trackers, q) do
    case :queue.out(q) do
      {:empty, _} ->
        {state, trackers}

      {{:value, {:broadcaster, :plain, false, src}}, q} ->
        send_pulses(
          modules,
          state,
          track_signal(trackers, false, src),
          :queue.join(
            q,
            :queue.from_list(
              modules[:broadcaster][:out]
              |> Enum.map(fn next -> {next, modules[next][:type], false, :broadcaster} end)
            )
          )
        )

      {{:value, {_, nil, on, src}}, q} ->
        send_pulses(
          modules,
          state,
          track_signal(trackers, on, src),
          q
        )

      {{:value, {_, :flip_flop, true, src}}, q} ->
        send_pulses(modules, state, track_signal(trackers, true, src), q)

      {{:value, {dst, :flip_flop, false, src}}, q} ->
        on = not state[dst]

        send_pulses(
          modules,
          %{state | dst => on},
          track_signal(trackers, false, src),
          :queue.join(
            q,
            :queue.from_list(
              modules[dst][:out]
              |> Enum.map(fn next -> {next, modules[next][:type], on, dst} end)
            )
          )
        )

      {{:value, {dst, :conjunction, on, src}}, q} ->
        inputs = Map.put(state[dst], src, on)
        output = Map.values(inputs) |> Enum.any?(&(not &1))

        send_pulses(
          modules,
          %{state | dst => inputs},
          track_signal(trackers, on, src),
          :queue.join(
            q,
            :queue.from_list(
              modules[dst][:out]
              |> Enum.map(fn next -> {next, modules[next][:type], output, dst} end)
            )
          )
        )
    end
  end

  defp track_button_press(trackers) do
    if is_integer(trackers[:button]) do
      %{trackers | button: trackers[:button] + 1}
    else
      trackers
    end
  end

  defp track_signal(trackers, on, src) do
    trackers =
      if Map.has_key?(trackers, on) do
        %{trackers | on => trackers[on] + 1}
      else
        trackers
      end

    trackers =
      case {on, Map.get(trackers, {:src, src})} do
        {true, 0} -> Map.put(trackers, {:src, src}, trackers[:button])
        _ -> trackers
      end

    trackers
  end

  def find_cycle(modules, state, trackers) do
    {state, trackers} = press_button(modules, state, trackers)

    if key_module_counts(trackers)
       |> Enum.all?(fn {_, n} -> n > 0 end) do
      key_module_counts(trackers)
    else
      find_cycle(modules, state, trackers)
    end
  end

  defp key_module_counts(trackers) do
    trackers
    |> Enum.filter(fn
      {{:src, _}, _} -> true
      _ -> false
    end)
    |> Map.new()
  end

  @doc """
  iex> "broadcaster -> a, b, c
  ...>%a -> b
  ...>%b -> c
  ...>%c -> inv
  ...>&inv -> a
  ...>" |> Dec20.a()
  32000000
  iex> "broadcaster -> a
  ...>%a -> inv, con
  ...>&inv -> b
  ...>%b -> con
  ...>&con -> output
  ...>" |> Dec20.a()
  11687500
  """
  def a(input) do
    modules = input |> parse()
    state = initial_state(modules)

    trackers =
      press_button(
        modules,
        state,
        %{true => 0, false => 0},
        1000
      )

    trackers[true] * trackers[false]
  end

  def b(input) do
    modules = input |> parse()
    state = initial_state(modules)

    key_modules = modules[modules[:rx][:in] |> Enum.at(0)][:in]

    trackers =
      key_modules
      |> Enum.map(fn k -> {{:src, k}, 0} end)
      |> Map.new()

    trackers = find_cycle(modules, state, Map.put(trackers, :button, 0))

    trackers |> Enum.map(fn {_, n} -> n end) |> Enum.reduce(1, &ElixirMath.lcm/2)
  end
end
