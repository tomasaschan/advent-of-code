defmodule Dec19 do
  defmodule Part do
    defstruct [:x, :m, :a, :s]

    def parse(part) do
      %{"x" => x, "m" => m, "a" => a, "s" => s} =
        Regex.named_captures(~r/^\{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)\}$/, part)

      %Part{
        x: String.to_integer(x),
        m: String.to_integer(m),
        a: String.to_integer(a),
        s: String.to_integer(s)
      }
    end

    def get(part, :x), do: part.x
    def get(part, :m), do: part.m
    def get(part, :a), do: part.a
    def get(part, :s), do: part.s
  end

  defmodule Workflow do
    defp parse_rule(input) do
      case Regex.named_captures(
             ~r/^(?<redirect>[a-zAR]+)$|^(?<field>[xmas])(?<op>[<>])(?<value>\d+):(?<next>[a-zAR]+)$/,
             input
           ) do
        %{"field" => field, "op" => ">", "value" => value, "next" => next} ->
          {String.to_atom(field), :gt, String.to_integer(value), String.to_atom(next)}

        %{"field" => field, "op" => "<", "value" => value, "next" => next} ->
          {String.to_atom(field), :lt, String.to_integer(value), String.to_atom(next)}

        %{"redirect" => redirect} ->
          String.to_atom(redirect)

        nil ->
          raise "invalid rule: #{input}"
      end
    end

    def parse(workflow) do
      %{"name" => name, "rules" => rules} =
        Regex.named_captures(~r/^(?<name>[a-z]+)\{(?<rules>.+)\}$/, workflow)

      {String.to_atom(name), rules |> String.split(",") |> Enum.map(&parse_rule/1)}
    end

    def next([rule | rest], part) do
      case rule do
        {field, :gt, value, nxt} ->
          if Part.get(part, field) > value do
            nxt
          else
            next(rest, part)
          end

        {field, :lt, value, nxt} ->
          if Part.get(part, field) < value do
            nxt
          else
            next(rest, part)
          end

        redirect ->
          redirect
      end
    end
  end

  defmodule Processor do
    def accepted?(workflows, part), do: accepted?(workflows, part, :in)

    def accepted?(workflows, part, at) do
      case Workflow.next(workflows[at], part) do
        :R ->
          false

        :A ->
          true

        next ->
          accepted?(workflows, part, next)
      end
    end
  end

  def parse(input) do
    [workflows, parts] = input |> String.split("\n\n", trim: true)

    [
      workflows:
        workflows
        |> String.split("\n", trim: true)
        |> Enum.map(&Workflow.parse/1)
        |> Map.new(),
      parts: parts |> String.split("\n", trim: true) |> Enum.map(&Part.parse/1)
    ]
  end

  @doc """
  iex> "px{a<2006:qkq,m>2090:A,rfg}
  ...>pv{a>1716:R,A}
  ...>lnx{m>1548:A,A}
  ...>rfg{s<537:gd,x>2440:R,A}
  ...>qs{s>3448:A,lnx}
  ...>qkq{x<1416:A,crn}
  ...>crn{x>2662:A,R}
  ...>in{s<1351:px,qqz}
  ...>qqz{s>2770:qs,m<1801:hdj,R}
  ...>gd{a>3333:R,R}
  ...>hdj{m>838:A,pv}
  ...>
  ...>{x=787,m=2655,a=1222,s=2876}
  ...>{x=1679,m=44,a=2067,s=496}
  ...>{x=2036,m=264,a=79,s=2244}
  ...>{x=2461,m=1339,a=466,s=291}
  ...>{x=2127,m=1623,a=2188,s=1013}
  ...>" |> Dec19.a()
  19114
  """
  def a(input) do
    input
    |> parse()
    |> then(fn [workflows: workflows, parts: parts] ->
      parts
      |> Enum.filter(&Processor.accepted?(workflows, &1))
    end)
    |> Enum.map(fn %Part{x: x, m: m, a: a, s: s} -> x + m + a + s end)
    |> Enum.sum()
  end
end
