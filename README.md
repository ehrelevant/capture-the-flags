[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/SsQ6IvG7)

# Corner The Flags Game

This project has a Python client and a PureScript client. To start a game, connect two of either client to a server.


> [!CAUTION]
> Neither client currently sends an initial message containing initial board dimensions and piece positions. Thus, clients that have different board states upon connecting to a room will not be able to sync with each other.

## Python Client

The Python client's dependencies are managed using [`poetry`](https://github.com/python-poetry/poetry).

```bash
# Installs dependencies (Poetry)
poetry install
```

### Linting & Formatting

Before pushing a commit, you may want to run the [`ruff`](https://github.com/astral-sh/ruff) linter and formatter.

```bash
# Runs linter (Ruff)
poetry run ruff check

# Runs formatter (Ruff)
poetry run ruff format
```

### Running Tests

The [`pytest`](https://github.com/pytest-dev/pytest) framework is used to write and run tests. These tests may be executed by running:

```bash
# Executes tests (Pytest)
poetry run pytest
```

## Purescript Client

Package management and building for the PureScript client are handled by [`spago`](https://github.com/purescript/spago).

```bash
# Installs dependencies (Spago)
spago install
```

A `MakeFile` is included to make building the PureScript easier.

```bash
# same as `spago bundle --outfile web/index.js`
make build
```

> [!NOTE]
> The above commands must be run in their respective client's directory.
