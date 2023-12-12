defmodule Dec10Test do
  use ExUnit.Case
  doctest Dec10

  test "Dec 10" do
    input = File.read!("../../inputs/2023/10.txt")
    assert Dec10.a(input) == 6968
  end
end
