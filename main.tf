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
  default = 5
}

resource "docker_network" "private_network" {
  name = "uu-net"
}

resource "docker_volume" "uu-nginx-configs" {}

resource "docker_image" "nats" {
  name = "nats:latest"
  keep_locally = true
}

resource "docker_image" "elixir" {
  name = "elixir:1.10.4"
  keep_locally = true
}

resource "docker_image" "nginx" {
  name = "nginx:latest"
  keep_locally = true
}

resource "docker_image" "consul" {
  name = "consul:latest"
  keep_locally = true
}

resource "docker_image" "consul-template" {
  name = "hashicorp/consul-template:alpine"
  keep_locally = true
}

resource "docker_container" "nats" {
  image = docker_image.nats.latest
  name  = "uu-nats"
  networks_advanced {
    name = "uu-net"
  }
}

resource "docker_container" "nginx" {
  image = docker_image.nginx.latest
  name  = "uu-nginx"
  networks_advanced {
    name = "uu-net"
  }
  mounts {
    type = "volume"
    target = "/etc/nginx/conf.d"
    source = docker_volume.uu-nginx-configs.name
  }
  depends_on = [
    docker_container.consul-template
  ]
  ports {
    internal = 80
    external = 8000
  }
}

resource "docker_container" "consul-template" {
  image = docker_image.consul-template.latest
  name  = "uu-consul-template"
  user = "root"

  networks_advanced {
    name = "uu-net"
  }

  mounts {
    type = "volume"
    target = "/out"
    source = docker_volume.uu-nginx-configs.name
  }

  mounts {
    type = "bind"
    target = "/templates"
    source = "${path.cwd}/templates"
  }
  depends_on = [
    docker_container.consul,
    docker_container.app
  ]
  command = [
    "/bin/consul-template",
    "-consul-addr", "${docker_container.consul.name}:8500",
    "-template", "/templates/nginx.ctmpl:/out/default.conf"
  ]
}

resource "docker_container" "consul" {
  image = docker_image.consul.latest
  name  = "uu-consul"
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
  hostname = "uu-app-${count.index}.local"
  command = [
    "mix", "do",
    "local.hex", "--force", ",",
    "local.rebar", "--force", ",",
    "run", "--no-halt", "--no-compile", "--no-deps-check"
  ]
  count = var.shards_count
  name  = "uu-app-${count.index}"
  env = [
    "CONSUL_ADDRESS=http://${docker_container.consul.name}:8500",
    "NATS_HOST=${docker_container.nats.name}",
    "SHARD_NUMBER=${count.index}",
    "SHARDS_COUNT=${var.shards_count}"
  ]
  depends_on = [
    docker_container.consul
  ]
}
