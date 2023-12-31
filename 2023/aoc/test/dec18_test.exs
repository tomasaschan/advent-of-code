defmodule Dec18Test do
  use ExUnit.Case
  doctest Dec18
  doctest Dec18.Instruction

  test "Dec 18" do
    input = File.read!("../../inputs/2023/18.txt")

    assert Dec18.a(input) == 52231
  end
end
