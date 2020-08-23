import Config

config :ubiquitous_umbrella,
  gnat_connection_settings: [
    %{host: '127.0.0.1', port: 4222}
  ]

config :ubiquitous_umbrella,
  gnat_subject: "pokemons.*"
