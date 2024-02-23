defmodule Dec24Test do
  use ExUnit.Case
  doctest Dec24

  test "Using correct backend" do
    {backend, _config} = Nx.default_backend()
    assert backend == EXLA.Backend
  end

  @tag timeout: :infinity
  test "Dec 24 a" do
    input = File.read!("../../inputs/2023/24.txt")

    assert Dec24.a(input) == 15889
    assert Dec24.b(input) == 801_386_475_216_902
  end
end
