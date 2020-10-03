import Config

config :ubiquitous_umbrella,
  gnat_connection_settings: [
    %{host: 'uu-nats', port: 4222}
  ]

config :ubiquitous_umbrella,
  gnat_subject: "pokemons.*"
