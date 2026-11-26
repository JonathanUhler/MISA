from pytest import Config, FixtureRequest, MarkDecorator, Metafunc, Parser, fixture, mark

from generators import (
    CAMPAIGN_SEEDS, PROFILE_SEEDS, Seed, SeedProgram, seed_program, seed_state
)
from memory import FixedLatency, MemMap, MemoryLatency, RandomLatency, ZeroLatency
from spec import SpecDriver
from target import AmaranthTarget, Target


TARGETS: dict = {
    "amaranth": AmaranthTarget
}

LATENCIES: dict = {
    "zero": ZeroLatency,
    "fixed1": lambda: FixedLatency(1),
    "fixed4": lambda: FixedLatency(4),
    "random": lambda: RandomLatency(1, 4)
}

SEED_COUNTS: dict = {
    "campaign_seed": CAMPAIGN_SEEDS,
    "profile_seed": PROFILE_SEEDS
}


def pytest_addoption(parser: Parser) -> None:
    parser.addoption("--target", default = "amaranth", choices = sorted(TARGETS),
                     help = "which level to run against (default: amaranth)")
    parser.addoption("--compare", default = "lockstep", choices = ("lockstep", "final"),
                     help = "compare after every instruction, or only the final state")
    parser.addoption("--latency", default = "zero", choices = sorted(LATENCIES),
                     help = "memory latency profile for MisaCore memory accesses")
    parser.addoption("--exhaustive", action = "store_true",
                     help = "run the full campaign sizes instead of the quick ones")
    parser.addoption("--seeds", type = int, default = None,
                     help = "how many seeds to use per campaign (default: campaign-specific)")


def pytest_configure(config: Config) -> None:
    for marker, description in (
        ("subset", "part of the verilog/gates subset"),
        ("exhaustive", "only runs under --exhaustive"),
        ("random", "randomised campaign"),
        ("equivalence", "formal or netlist equivalence"),
        ("slow", "takes more than a few seconds")
    ):
        config.addinivalue_line("markers", f"{marker}: {description}")


def pytest_generate_tests(metafunc: Metafunc) -> None:
    for name, (quick, full) in SEED_COUNTS.items():
        if (name not in metafunc.fixturenames):
            continue
        count: int = metafunc.config.getoption("--seeds")
        if (count is None):
            count = full if (metafunc.config.getoption("--exhaustive")) else quick
        metafunc.parametrize(name, range(count), ids = lambda seed: f"seed{seed}")


def pytest_collection_modifyitems(config: Config, items: list) -> None:
    if (config.getoption("--exhaustive")):
        return
    skip_exhaustive: MarkDecorator = mark.skip(reason = "only runs under --exhaustive")
    for item in items:
        if ("exhaustive" in item.keywords):
            item.add_marker(skip_exhaustive)


@fixture(scope = "session")
def spec() -> SpecDriver:
    return SpecDriver()


@fixture(scope = "session")
def target(request: FixtureRequest) -> Target:
    return TARGETS[request.config.getoption("--target")]()


@fixture
def latency(request: FixtureRequest) -> MemoryLatency:
    return LATENCIES[request.config.getoption("--latency")]()


@fixture
def compare_mode(request: FixtureRequest) -> str:
    return request.config.getoption("--compare")


@fixture
def exhaustive(request: FixtureRequest) -> bool:
    return request.config.getoption("--exhaustive")


@fixture
def seed_count(request: FixtureRequest) -> int:
    return request.config.getoption("--seeds")


@fixture(scope = "session")
def everything() -> MemMap:
    return MemMap.everything()


@fixture(scope = "session")
def seeds(spec: SpecDriver) -> object:
    cache: dict = {}

    def resolve(seed: Seed, extns: int) -> tuple:
        key: tuple = (seed.name, extns)
        if (key not in cache):
            program: SeedProgram = seed_program(seed)
            cache[key] = (program, seed_state(spec, program, extns))
        return cache[key]

    return resolve
