# Gemini HawkEars Model Comparison Script 2026-09-29
# This script downloads a defined number of audio files at a time from a CSV list, runs them through multiple versions of the HawkEars model, and saves the results in separate directories for comparison.

import csv
import os
import subprocess
import urllib.request
from datetime import datetime
from pathlib import Path

# ==========================================
# 1. CONFIGURATION
# ==========================================
CSV_FILE = "recordings.csv"
URL_COL = "url"
LOCATION_COL = "location"
DATE_COL = "recording_date"

# Set your batch size here (3-5 is optimal)
CHUNK_SIZE = 3

# Set you minimum score threshold for detections (0.0 - 1.0)
MIN_SCORE = 0.5

today = datetime.now().strftime("%Y-%m-%d")
TEMP_AUDIO_DIR = Path("./temp_audio").resolve()
BASE_OUTPUT_DIR = (Path("./hawkears_results") / today).resolve()

TEMP_AUDIO_DIR.mkdir(exist_ok=True)
BASE_OUTPUT_DIR.mkdir(exist_ok=True)

HAWKEARS_VERSIONS = {
    "v1_0_8": {
        "repo_path": Path("C:/Users/Kevin/Documents/Repos/HawkEars"),
        "python_exe": Path("C:/Users/Kevin/Documents/Repos/HawkEars/.venv/Scripts/python.exe"),
        "type": "v1"
    },
    "v2_0_0": {
        "repo_path": Path("C:/Users/Kevin/Documents/Repos/HawkEars2"),
        "python_exe": Path("C:/Users/Kevin/Documents/Repos/HawkEars2/.venv/Scripts/python.exe"),
        "type": "v2"
    },
    "v2_3_0": {
        "repo_path": Path("C:/Users/Kevin/Documents/Repos/HawkEars2_3"),
        "python_exe": Path("C:/Users/Kevin/Documents/Repos/HawkEars2_3/venv/Scripts/python.exe"),
        "type": "v2"
    }
}

# Helper function to divide records into batches
def get_chunks(lst, n):
    for i in range(0, len(lst), n):
        yield lst[i:i + n]

# ==========================================
# 2. READ & PARSE CSV
# ==========================================
records = []
with open(CSV_FILE, mode="r", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    for row in reader:
        url = row.get(URL_COL, "").strip()
        location = row.get(LOCATION_COL, "").strip()
        raw_date = row.get(DATE_COL, "").strip()

        if url and location and raw_date:
            dt = datetime.strptime(raw_date, "%Y-%m-%d %H:%M:%S")
            formatted_date = dt.strftime("%Y%m%d_%H%M%S")
            ext = Path(url.split("?")[0]).suffix
            custom_filename = f"{location}_{formatted_date}{ext}"

            records.append({
                "url": url,
                "filename": custom_filename
            })

print(f"Loaded {len(records)} recording records from {CSV_FILE}.")

# ==========================================
# 3. BATCHED STREAMING PROCESSING LOOP
# ==========================================
batches = list(get_chunks(records, CHUNK_SIZE))

for batch_idx, batch in enumerate(batches, 1):
    print(f"\n==================================================")
    print(f"Processing Batch [{batch_idx}/{len(batches)}] ({len(batch)} files)")
    print(f"==================================================")

    # Clean leftover files from prior runs
    for leftover in TEMP_AUDIO_DIR.glob("*"):
        try:
            if leftover.is_file():
                leftover.unlink()
        except Exception:
            pass

    # Step A: Download all audio files in current batch
    downloaded_files = []
    for item in batch:
        local_file = TEMP_AUDIO_DIR / item["filename"]
        print(f"--> Downloading: {item['filename']}")
        try:
            urllib.request.urlretrieve(item["url"], local_file)
            downloaded_files.append(local_file)
        except Exception as e:
            print(f"    Download failed for {item['filename']}: {e}")

    if not downloaded_files:
        print("    No files downloaded in this batch. Skipping analysis.")
        continue

    # Step B: Run HawkEars models on the batch directory
    for ver_name, config in HAWKEARS_VERSIONS.items():
        repo_dir = config["repo_path"].resolve()
        python_exe = config["python_exe"].resolve()
        version_type = config["type"]

        run_output_dir = BASE_OUTPUT_DIR / ver_name
        run_output_dir.mkdir(exist_ok=True)

        if not repo_dir.exists() or not python_exe.exists():
            print(f"  [SKIPPED] {ver_name} paths invalid.")
            continue

        print(f"  > Running {ver_name} on batch...")

        if version_type == "v1":
            cmd = [
                str(python_exe), str(repo_dir / "analyze.py"),
                "-i", str(TEMP_AUDIO_DIR),
                "-o", str(run_output_dir),
                "--min_score", str(MIN_SCORE)
            ]
        else:
            hawkears_bin = python_exe.parent / "hawkears.exe"
            if hawkears_bin.exists():
                cmd = [
                    str(hawkears_bin), "analyze",
                    "-i", str(TEMP_AUDIO_DIR),
                    "-o", str(run_output_dir),
                    "--rtype", "csv+audacity",
                    "--min_score", str(MIN_SCORE)
                ]
            else:
                cmd = [
                    str(python_exe), "-m", "hawkears", "analyze",
                    "-i", str(TEMP_AUDIO_DIR),
                    "-o", str(run_output_dir),
                    "--rtype", "csv+audacity",
                    "--min_score", str(MIN_SCORE)
                ]

        result = subprocess.run(cmd, cwd=str(repo_dir), capture_output=True, text=True)

        if result.returncode != 0:
            print(f"    [ERROR {ver_name}]: {result.stderr.strip()}")
        elif result.stdout.strip():
            print(f"    [{ver_name} Output]: {result.stdout.strip()}")

    # Step C: Delete local temp audio files for this batch
    for local_file in downloaded_files:
        if local_file.exists():
            try:
                os.remove(local_file)
            except Exception as e:
                print(f"    Could not delete {local_file.name}: {e}")
    print(f"--> Batch {batch_idx} completed & temporary files cleared.")

print("\nProcessing finished! Results saved under 'hawkears_results/'.")