.PHONY: compile cluster

compile:
	docker run --rm -v `pwd`:/build -w /build elixir:1.10.4 mix do local.hex --force, deps.get, compile

cluster: compile
	terraform apply -auto-approve

client:
	docker run --rm -v `pwd`:/app -w /app --net=uu-net -it elixir:1.10.4 iex -S mix

clean:
	terraform destroy -auto-approve
	mix clean

run:
	iex -S mix
