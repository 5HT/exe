defmodule Homotopy.Mixfile do
  use Mix.Project
  def deps, do: [ {:ex_doc, "~> 0.11", only: :dev} ]
  def application, do: [mod: {:homotopy, []}, applications: []]
  def project do
    [ app: :homotopy,
      version: "1.3.1",
      description: "CTT-CCHM Homotopy Type System",
      package: package(),
      deps: deps()]
  end
  def package do
    [ files: ~w(doc priv src mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/groupoid/homotopy"}]
  end
end
