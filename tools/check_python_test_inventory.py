#!/usr/bin/env python3
"""Verify the pinned Python 1.0.0 behavioral-suite inventory.

The R tests intentionally group related Python cases into contract-focused
``test_that()`` blocks. This gate prevents a Python case from disappearing from
that mapping silently and verifies that every mapped R test file is present.
"""

from __future__ import annotations

import ast
import hashlib
import sys
from pathlib import Path


EXPECTED = {
    "test_compliance.py": (9, "665c09626ef9e29d82fcfe69b2d77073bb177e43956541dfc9542d4c736e855e", ["test-data-analysis-contracts.R"]),
    "test_imports.py": (2, "58ce7c02f5e41d18baf83c0e87da743308dc949f77ad04b9c15d36bef9f67ad4", ["test-io-contracts.R"]),
    "test_io_logs.py": (72, "1251b910b67fa38e59d4bbf71107577cc12783959de334ae61758b9bea191654", ["test-raw-log-parity.R", "test-conversion-contracts.R", "test-parity-fixtures.R"]),
    "test_io_manual_diary.py": (4, "4581039b469376f3c0d146ddfa995c970e0e38ba24c90a33c0275f9c2e9f8450", ["test-io-contracts.R", "test-conversion-contracts.R"]),
    "test_io_raw_log_folders.py": (8, "c24c91a5f4a6634e298919655e6a9f65b2be2ee3f5e4541f9ccdcf6cb5f07135", ["test-raw-log-parity.R", "test-io-contracts.R"]),
    "test_io_saliva.py": (10, "07a391e7a755b1d272968033de11f20bac08d171f282c46d1b509d4bfb27bed9", ["test-io-contracts.R"]),
    "test_io_study_results.py": (21, "ac7a42ea2ed59e3c7bcb758e77cafd62e6610ee6023b2b5de2bebdd9619d1124", ["test-results.R", "test-io-contracts.R"]),
    "test_issue_editor.py": (8, "53ccde845d1307b04a307bcf4bb02e4b800f6f333aeea687c580152e73d0beb4", ["test-interactive.R"]),
    "test_merge.py": (20, "794fcb96e75bffebbabf61647b950d29d514322cec97aacc65d7fc4c957e1745", ["test-data-analysis-contracts.R"]),
    "test_plotting.py": (17, "27c8e15aaa77c04fa0ce135d8977a078d41377a9da2a777f61b6c1f17e05fe06", ["test-data-analysis-contracts.R", "test-interactive.R"]),
    "test_public_documentation.py": (5, "2a9c06d192f566d0b0e2c29e8d22f905a1c7d08e592a2c85fc082d433a212da8", ["test-io-contracts.R"]),
    "test_saliva_metrics.py": (25, "fc8be9ae38708b886fd40527b0a70a14202281b8cce3ac953eeb1ba7bb8afb63", ["test-data-analysis-contracts.R"]),
    "test_sampling_quality.py": (3, "ab559d12bbde012ffcabb92f47db83fe7a699f56fa2aa53a5599e63fa219ed3c", ["test-data-analysis-contracts.R"]),
    "test_source_audit.py": (2, "92560045e37d71b2a27c41b76749cd5195738cb3d5e39930506a663b94a032b5", ["test-io-contracts.R"]),
    "test_synthetic_example_data.py": (10, "1a7516c8c3f631e0162dbe2947749516fea415f1c7dc3e67b14c87412a036d54", ["test-synthetic-contracts.R"]),
}


def test_names(path: Path) -> list[str]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    return sorted(
        node.name
        for node in ast.walk(tree)
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        and node.name.startswith("test_")
    )


def main() -> int:
    python_tests = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("../carwatch-python/tests")
    r_tests = Path(__file__).resolve().parents[1] / "tests" / "testthat"
    actual_modules = {path.name for path in python_tests.glob("test_*.py")}
    expected_modules = set(EXPECTED)
    failures: list[str] = []

    if actual_modules != expected_modules:
        failures.append(
            f"Python module inventory changed; missing={sorted(expected_modules - actual_modules)}, "
            f"unexpected={sorted(actual_modules - expected_modules)}"
        )

    total = 0
    for module, (expected_count, expected_hash, mapped_files) in EXPECTED.items():
        path = python_tests / module
        if not path.exists():
            continue
        names = test_names(path)
        total += len(names)
        digest = hashlib.sha256("\n".join(names).encode()).hexdigest()
        if len(names) != expected_count or digest != expected_hash:
            failures.append(
                f"{module}: expected {expected_count} cases/{expected_hash}, "
                f"found {len(names)} cases/{digest}"
            )
        for mapped_file in mapped_files:
            if not (r_tests / mapped_file).is_file():
                failures.append(f"{module}: mapped R test file is missing: {mapped_file}")

    if total != 216:
        failures.append(f"expected 216 Python tests, found {total}")

    if failures:
        print("\n".join(failures), file=sys.stderr)
        return 1
    print(f"Mapped all {total} Python 1.0.0 tests across {len(EXPECTED)} modules.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
