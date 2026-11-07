import pytest


def pytest_addoption(parser: pytest.Parser) -> None:
    parser.addoption("--exhaustive", action = "store_true", default = False,
                     help = "run the heavy exhaustive sweeps and high-seed-count random tests")


@pytest.fixture
def exhaustive(request: pytest.FixtureRequest) -> bool:
    return request.config.getoption("--exhaustive")


def pytest_generate_tests(metafunc: pytest.Metafunc) -> None:
    # Random co-sim tests request `cosim_seed`, the count scales with --exhaustive.
    if ("cosim_seed" in metafunc.fixturenames):
        count: int = 400 if metafunc.config.getoption("--exhaustive") else 24
        metafunc.parametrize("cosim_seed", range(count))
