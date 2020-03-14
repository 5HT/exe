defmodule HTS.Mixfile do
  use Mix.Project
  def deps, do: [ {:ex_doc, "~> 0.11", only: :dev} ]
  def application, do: [mod: {:cub, []}, applications: []]
  def project do
    [ app: :hts,
      version: "1.3.0",
      description: "HTS CTT-CCHM Groupoid Infinity",
      package: package(),
      deps: deps()]
  end
  def package do
    [ files: ~w(doc priv src mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/groupoid/hts"}]
  end
end
