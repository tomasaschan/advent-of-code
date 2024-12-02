defmodule Dec25Test do
  use ExUnit.Case
  doctest Dec25

  test "Dec 25" do
    input = File.read!("../../inputs/2023/25.txt")

    assert Dec25.a(input) == 0
  end
end
