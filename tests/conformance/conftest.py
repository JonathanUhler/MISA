"""
Pytest configuration for the MISA conformance suite.

The presence of this file makes the conformance directory importable, so tests can import the
harness module directly. It also skips the whole suite with a clear message when the toolchain has
not been built yet.
"""


import pytest

from harness import INSTALL_DIR


@pytest.fixture(autouse = True, scope = "session")
def require_toolchain():
    """
    Skips the suite when the installed toolchain is missing.

    The conformance tests shell out to the misa-as wrapper in the install directory. When that
    directory has not been populated by a build, there is nothing to test against, so the suite is
    skipped rather than failed.
    """

    if (not (INSTALL_DIR / "misa-as").exists()):
        pytest.skip(f"toolchain not built, run make first (expected {INSTALL_DIR / 'misa-as'})")
