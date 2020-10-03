terraform {
  required_providers {
    docker = {
      source = "terraform-providers/docker"
    }
  }
}

provider "docker" {}

variable "shards_count" {
  type = number
  default = 2
}

resource "docker_network" "private_network" {
  name = "uu-net"
}

resource "docker_image" "nats" {
  name = "nats:latest"
  keep_locally = true
}

resource "docker_image" "elixir" {
  name = "elixir:1.10.4"
  keep_locally = true
}

resource "docker_container" "nats" {
  image = docker_image.nats.latest
  name  = "uu-nats"
  networks_advanced {
    name = "uu-net"
  }
}

resource "docker_container" "app" {
  image = docker_image.elixir.latest
  mounts {
    type = "bind"
    target = "/app"
    source = path.cwd
  }
  networks_advanced {
    name = "uu-net"
  }
  working_dir = "/app"
  command = [
    "mix", "do",
    "local.hex", "--force", ",",
    "run", "--no-halt", "--no-compile", "--no-deps-check"
  ]
  count = var.shards_count
  name  = "uu-app-${count.index}"
  env = [
    "SHARD_NUMBER=${count.index}",
    "SHARDS_COUNT=${var.shards_count}"
  ]
}
