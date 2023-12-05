defmodule Dec01Test do
  use ExUnit.Case

  doctest Dec01

  test "dec 01" do
    input = File.read!("../../inputs/2023/01.txt")
    assert Dec01.a(input) == 55386
    assert Dec01.b(input) == 54824
  end
end
