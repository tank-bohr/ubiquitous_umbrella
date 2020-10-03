import Config

nats_host = System.get_env("NATS_HOST", "localhost")

config :ubiquitous_umbrella,
  gnat_connection_settings: [
    %{host: to_charlist(nats_host), port: 4222}
  ]

config :ubiquitous_umbrella,
  gnat_group: "pokemons.group.*",
  gnat_subject: "pokemons.*"
