defmodule Dec20 do
  defmodule Modules do
    def flip_flop(name, nexts) do
      pid = spawn_link(fn -> flip_flop_loop({false, name, nexts}) end)
      Process.register(pid, name)
    end

    defp flip_flop_loop({on, name, nexts}) do
      receive do
        {:init} ->
          nexts |> Enum.each(fn next -> send(next, {:connect, name}) end)
          flip_flop_loop({on, name, nexts})

        {:connect, _} ->
          flip_flop_loop({on, name, nexts})

        {:signal, true, _} ->
          flip_flop_loop({on, name, nexts})

        {:signal, false, _} ->
          send(:bus, nexts |> Enum.map(fn next -> {next, {:signal, not on, name}} end))
          flip_flop_loop({not on, name, nexts})

        {:shut_down, pid} ->
          send(pid, :ok)
      end
    end

    def conjunction(name, nexts) do
      pid = spawn_link(fn -> conjunction_loop({%{}, name, nexts}) end)
      Process.register(pid, name)
    end

    defp conjunction_loop({inputs, name, nexts}) do
      receive do
        {:init} ->
          nexts |> Enum.each(fn next -> send(next, {:connect, name}) end)
          conjunction_loop({inputs, name, nexts})

        {:connect, input} ->
          conjunction_loop({Map.put(inputs, input, false), name, nexts})

        {:signal, on, src} ->
          inputs = Map.put(inputs, src, on)
          output = Map.values(inputs) |> Enum.any?(&(not &1))
          send(:bus, nexts |> Enum.map(fn next -> {next, {:signal, output, name}} end))
          conjunction_loop({inputs, name, nexts})

        {:shut_down, pid} ->
          send(pid, :ok)
      end
    end

    def broadcaster(nexts) do
      pid = spawn_link(fn -> broadcaster_loop({nexts}) end)
      Process.register(pid, :broadcaster)
    end

    defp broadcaster_loop({nexts}) do
      receive do
        {:init} ->
          nexts |> Enum.each(fn next -> send(next, {:connect, :broadcaster}) end)
          broadcaster_loop({nexts})

        {:connect, _} ->
          broadcaster_loop({nexts})

        {:signal, on, _} ->
          send(:bus, nexts |> Enum.map(fn next -> {next, {:signal, on, :broadcaster}} end))
          broadcaster_loop({nexts})

        {:shut_down, pid} ->
          send(pid, :ok)
      end
    end

    def push_button() do
      send(:bus, [{:broadcaster, {:signal, false, :button}}])
    end

    def bus({runner, trace}) do
      pid = spawn_link(fn -> bus_loop({0, 0, runner, trace}) end)
      Process.register(pid, :bus)
    end

    defp bus_loop({lo, hi, runner, trace}) do
      receive do
        {:shut_down, pid} ->
          send(pid, :ok)

        signals ->
          counts =
            signals
            |> Enum.group_by(fn {_, {:signal, on, _}} -> on end)
            |> Map.to_list()
            |> Enum.map(fn {on, signals} -> {on, signals |> Enum.count()} end)
            |> Map.new()

          signals
          |> Enum.each(fn {next, {:signal, on, src}} ->
            if trace do
              IO.puts(
                "#{src} -#{if on do
                  "high"
                else
                  "low"
                end}-> #{next}"
              )
            end

            send(next, {:signal, on, src})
          end)

          bus_loop({lo + Map.get(counts, false, 0), hi + Map.get(counts, true, 0), runner, trace})
      after
        5 ->
          send(runner, {lo, hi})
          bus_loop({lo, hi, runner, trace})
      end
    end

    def output(name) do
      pid = spawn_link(fn -> output_loop() end)
      Process.register(pid, name)
    end

    defp output_loop() do
      receive do
        {:init} ->
          output_loop()

        {:connect, _} ->
          output_loop()

        {:signal, _, _} ->
          output_loop()

        {:shut_down, pid} ->
          send(pid, :ok)
      end
    end
  end

  defmodule Parse do
    @doc """
    iex> "broadcaster -> a, b, c
    ...>%a -> b
    ...>%b -> c
    ...>%c -> inv
    ...>&inv -> a
    ...>" |> Dec20.Parse.modules()
    [
      {:broadcaster, [:a, :b, :c]},
      {:flip_flop, :a, [:b]},
      {:flip_flop, :b, [:c]},
      {:flip_flop, :c, [:inv]},
      {:conjunction, :inv, [:a]}
    ]
    iex> "broadcaster -> a
    ...>%a -> inv, con
    ...>&inv -> b
    ...>%b -> con
    ...>&con -> output
    ...>" |> Dec20.Parse.modules()
    [
      {:broadcaster, [:a]},
      {:flip_flop, :a, [:inv, :con]},
      {:conjunction, :inv, [:b]},
      {:flip_flop, :b, [:con]},
      {:conjunction, :con, [:output]},
      {:output, :output}
    ]
    """
    def modules(input) do
      modules =
        input
        |> String.split("\n", trim: true)
        |> Enum.map(&parse_module/1)

      known =
        modules
        |> Enum.map(fn
          {:broadcaster, _} -> :broadcaster
          {:flip_flop, name, _} -> name
          {:conjunction, name, _} -> name
        end)
        |> MapSet.new()

      outputs =
        modules
        |> Enum.flat_map(fn
          {:broadcaster, nexts} -> nexts
          {:flip_flop, _, nexts} -> nexts
          {:conjunction, _, nexts} -> nexts
        end)
        |> MapSet.new()

      modules
      |> Enum.concat(
        MapSet.difference(outputs, known)
        |> Enum.map(fn name -> {:output, name} end)
      )
    end

    defp parse_module("%" <> line) do
      [name, nexts] = line |> String.split(" -> ", trim: true)
      nexts = nexts |> String.split(", ", trim: true) |> Enum.map(&String.to_atom/1)
      {:flip_flop, String.to_atom(name), nexts}
    end

    defp parse_module("broadcaster -> " <> nexts) do
      nexts = nexts |> String.split(", ", trim: true) |> Enum.map(&String.to_atom/1)
      {:broadcaster, nexts}
    end

    defp parse_module("&" <> line) do
      [name, nexts] = line |> String.split(" -> ", trim: true)
      nexts = nexts |> String.split(", ", trim: true) |> Enum.map(&String.to_atom/1)
      {:conjunction, String.to_atom(name), nexts}
    end
  end

  defmodule Run do
    defp start(modules, trace) do
      Modules.bus({self(), trace})

      modules
      |> Enum.each(fn
        {:broadcaster, nexts} -> Modules.broadcaster(nexts)
        {:flip_flop, name, nexts} -> Modules.flip_flop(name, nexts)
        {:conjunction, name, nexts} -> Modules.conjunction(name, nexts)
        {:output, name} -> Modules.output(name)
      end)

      modules
      |> Enum.each(fn
        {:broadcaster, _} -> send(:broadcaster, {:init})
        {:flip_flop, name, _} -> send(name, {:init})
        {:conjunction, name, _} -> send(name, {:init})
        {:output, _} -> :ok
      end)
    end

    defp stop(modules) do
      modules
      |> Enum.each(fn
        {:broadcaster, _} ->
          send(:broadcaster, {:shut_down, self()})

        {:output, name} ->
          send(name, {:shut_down, self()})

        {_, name, _} ->
          send(name, {:shut_down, self()})
      end)

      send(:bus, {:shut_down, self()})

      receive do
      after
        100 ->
          :ok
      end
    end

    @doc """
    iex> "broadcaster -> a
    ...>%a -> inv, con
    ...>&inv -> b
    ...>%b -> con
    ...>&con -> output
    ...>" |> Dec20.Parse.modules() |> Dec20.Run.run(1)
    {4, 4}
    iex> "broadcaster -> a
    ...>%a -> inv, con
    ...>&inv -> b
    ...>%b -> con
    ...>&con -> output
    ...>" |> Dec20.Parse.modules() |> Dec20.Run.run(4)
    {17, 11}
    """
    def run(modules, n, trace \\ false) do
      start(modules, trace)

      answer = run_loop({n, trace})

      stop(modules)

      answer
    end

    defp run_loop({0, _}) do
      receive do
        {lo, hi} -> {lo, hi}
      end
    end

    defp run_loop({n, trace}) do
      receive do
        {_, _} ->
          if trace do
            IO.puts("")
          end

          Modules.push_button()
          run_loop({n - 1, trace})
      end
    end
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
    {lo, hi} = input |> Parse.modules() |> Run.run(1000)
    lo * hi
  end
end
