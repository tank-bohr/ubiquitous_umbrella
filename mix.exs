defmodule UbiquitousUmbrella.MixProject do
  use Mix.Project

  def project do
    [
      app: :ubiquitous_umbrella,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {:ubiquitous_umbrella_app, []}
    ]
  end

  defp deps do
    [
      {:jsx, "~> 3.0"},
      {:gnat, "~> 1.1"},
      {:backoff, "~> 1.1"},
      {:cowboy, "~> 2.8"}
    ]
  end
end
