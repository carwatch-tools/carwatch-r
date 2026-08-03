"""Generate versioned R-port parity fixtures with carwatch-python 1.0.0."""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd

import carwatch as cw


ROOT = Path(__file__).resolve().parents[1] / "inst" / "extdata" / "parity" / "v1.0.0"


def line(timestamp: int, action: str, payload: dict) -> str:
    return f"{timestamp};local;{action};{json.dumps(payload, separators=(',', ':'))}\n"


def main() -> None:
    ROOT.mkdir(parents=True, exist_ok=True)
    raw_dir = ROOT / "raw" / "VP01"
    raw_dir.mkdir(parents=True, exist_ok=True)
    metadata = {
        "study_name": "parity-study",
        "saliva_ids": ["tube-a", "tube-b"],
        "saliva_times": [0, 30],
        "study_days": 1,
    }
    contents = "".join(
        [
            line(1747281600000, "study_metadata", metadata),
            line(1747282000000, "spontaneous_awakening", {"id": 0}),
            line(1747282000000, "barcode_scanned", {"sample_expected": "tube-a", "sample_scanned": "tube-a", "barcode_value": "barcode-a", "day_expected": 1, "day_scanned": 1}),
            line(1747283800000, "barcode_scanned", {"sample_expected": "tube-b", "sample_scanned": "tube-b", "barcode_value": "barcode-b", "day_expected": 1, "day_scanned": 1}),
        ]
    )
    (raw_dir / "carwatch_parity_VP01_20250515.csv").write_text(contents)
    logs, audit = cw.io.load_raw_logs_from_participant_folders({"VP01": raw_dir}, create_report=True)
    results, report = cw.logs.convert_raw_logs_to_study_manager_summary(logs, errors="warn", create_report=True)
    cw.io.save_study_results(results, ROOT / "results.csv")
    report["issues"].to_csv(ROOT / "issue_report.csv")
    audit.to_csv(ROOT / "source_audit.csv", index=False)
    saliva = pd.DataFrame({"participant": ["VP01", "VP01"], "sample": ["tube-a", "tube-b"], "cortisol": [5.0, 9.0]})
    saliva.to_csv(ROOT / "saliva.csv", index=False)
    merged = cw.merge.merge_saliva(results, saliva.set_index(["participant", "sample"]))
    cw.io.save_study_results(merged, ROOT / "merged_results.csv")
    cw.compliance.summarize_compliance(merged).to_csv(ROOT / "compliance.csv", index=False)
    cw.saliva.compute_features_from_carwatch(merged).to_csv(ROOT / "features.csv")
    (ROOT / "manifest.json").write_text(json.dumps({"oracle": "carwatch-python", "oracle_version": "1.0.0", "fixture_schema": 1, "artifacts": ["results.csv", "issue_report.csv", "source_audit.csv", "saliva.csv", "merged_results.csv", "compliance.csv", "features.csv"]}, indent=2) + "\n")


if __name__ == "__main__":
    main()
