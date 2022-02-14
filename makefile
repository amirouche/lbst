help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

init: ## Use the system python3 to create a virtual environment with poetry (requires curl).
	curl -sSL https://install.python-poetry.org | python3 -
	POETRY_VIRTUALENVS_PATH=venv poetry install
	POETRY_VIRTUALENVS_PATH=venv poetry shell

check: ## Run tests, and security audit.
	POETRY_VIRTUALENVS_PATH=venv poetry run pytest -vvv --capture=no --cov-report=term --cov-report=html --cov=sh tests.py
	# XXX: ignore the use of assert B101, and the non-cryptographic quality of random B311
	POETRY_VIRTUALENVS_PATH=venv poetry run bandit --skip=B101,B311 sh.py

check-fast: ## Run tests, exit on first error.
	POETRY_VIRTUALENVS_PATH=venv poetry run pytest --exitfirst -vvv --capture=no tests.py

lint: ## Run linter.
	POETRY_VIRTUALENVS_PATH=venv poetry run pylama sh.py

todo: ## Things that should be done...
	@grep -nR --color=always  --before-context=2  --after-context=2 TODO sh.py

xxx: ## Things that require attention!
	@grep -nR --color=always --before-context=2  --after-context=2 XXX sh.py

release: check ## Let there be releases (to use with care).
	POETRY_VIRTUALENVS_PATH=venv poetry run poetry export -f requirements.txt --output requirements.txt
	POETRY_VIRTUALENVS_PATH=venv poetry run black sh.py
	POETRY_VIRTUALENVS_PATH=venv poetry run black tests.py
