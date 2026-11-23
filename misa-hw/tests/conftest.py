from pathlib import Path

from pytest import Config, FixtureRequest, Parser, fixture

from memory import FixedLatency, MemMap, MemoryLatency, RandomLatency, ZeroLatency
from spec import SpecDriver
from target import AmaranthTarget, Target


REPO_ROOT: Path = Path(__file__).resolve().parents[2]
BRINGUP_DIR: Path = REPO_ROOT / "programs" / "bringup"

TARGETS: dict = {
    "amaranth": AmaranthTarget
}

LATENCIES: dict = {
    "zero": ZeroLatency,
    "fixed1": lambda: FixedLatency(1),
    "fixed4": lambda: FixedLatency(4),
    "random": lambda: RandomLatency(1, 4)
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
def exhaustive(request: FixtureRequest) -> bool:
    return request.config.getoption("--exhaustive")


@fixture(scope = "session")
def everything() -> MemMap:
    return MemMap.everything()
