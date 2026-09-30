"""Capture and verify the public OSF baseline without modifying local results.

Run from the repository root. State is stored below resources/migration, outside
the package and Git. No credentials are required for public OSF downloads.
"""
import argparse
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import shutil
import sys
import time

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "resources" / "migration" / "python-libs"))
import requests

ROOT = Path(__file__).resolve().parents[1]
STATE = ROOT / "resources" / "migration"
NODES = {"no_bias": "q8phr", "Alinaghi2018": "5hbm8", "Bom2019": "4bcr2",
         "Carter2019": "vcs85", "Stanley2017": "fg62w"}


def atomic_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, ensure_ascii=False), encoding="utf-8")
    os.replace(temporary, path)


def request(url, **kwargs):
    for attempt in range(8):
        try:
            response = requests.get(url, timeout=(30, 180), **kwargs)
            if response.status_code in (429, 500, 502, 503, 504):
                response.close()
                time.sleep(min(60, 2 ** attempt))
                continue
            response.raise_for_status()
            return response
        except requests.RequestException:
            if attempt == 7:
                raise RuntimeError("Public OSF request failed after eight attempts") from None
            time.sleep(min(60, 2 ** attempt))
    raise RuntimeError("Public OSF request exhausted its retry limit")


def listing(url):
    # OSF's default listing order is unstable between pages. An explicit order
    # prevents duplicates and skipped files at pagination boundaries.
    url += ("&" if "?" in url else "?") + "page[size]=100&sort=name"
    seen = set()
    while url:
        page = request(url).json()
        for entry in page["data"]:
            if entry["id"] in seen:
                raise RuntimeError("OSF pagination repeated a file; inventory is incomplete")
            seen.add(entry["id"])
            yield entry
        url = page.get("links", {}).get("next")


def inventory_node(item):
    dgm, node = item
    node_metadata = request(f"https://api.osf.io/v2/nodes/{node}/").json()["data"]
    assets = []

    def walk(url, prefix=""):
        for entry in listing(url):
            attr = entry["attributes"]
            name = prefix + attr["name"]
            if attr["kind"] == "folder":
                walk(entry["relationships"]["files"]["links"]["related"]["href"], name + "/")
            else:
                hashes = attr.get("extra", {}).get("hashes", {})
                if not hashes.get("sha256") or not hashes.get("md5"):
                    raise RuntimeError(f"Missing source hashes: {dgm}/{name}")
                assets.append({"dgm": dgm, "path": name, "kind": name.split("/")[0],
                               "size": attr["size"], "sha256": hashes["sha256"], "md5": hashes["md5"],
                               "osf_id": entry["id"], "osf_node": node,
                               "osf_version": attr["current_version"],
                               "modified": attr["date_modified"], "url": entry["links"]["download"]})
    walk(f"https://api.osf.io/v2/nodes/{node}/files/osfstorage/")
    print(f"Inventoried {dgm}: {len(assets)} files", flush=True)
    return assets, {"attributes": node_metadata["attributes"], "relationships": node_metadata["relationships"]}


def inventory():
    assets, nodes = [], {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        for (dgm, _), (node_assets, metadata) in zip(NODES.items(), pool.map(inventory_node, NODES.items())):
            assets.extend(node_assets)
            nodes[dgm] = metadata
    root = request("https://api.osf.io/v2/nodes/exf3m/").json()["data"]
    license_url = root["relationships"]["license"]["links"]["related"]["href"]
    value = {"captured_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
             "source": "https://osf.io/exf3m/", "license": request(license_url).json()["data"],
             "nodes": nodes, "assets": sorted(assets, key=lambda x: (x["dgm"], x["path"]))}
    atomic_json(STATE / "inventory.json", value)
    print(f"Captured {len(assets)} files, {sum(x['size'] for x in assets) / 1e9:.3f} GB", flush=True)


def hashes(path):
    sha, md5 = hashlib.sha256(), hashlib.md5()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(4 * 1024 * 1024), b""):
            sha.update(block)
            md5.update(block)
    return sha.hexdigest(), md5.hexdigest()


def valid(path, asset):
    return (path.is_file() and path.stat().st_size == asset["size"]
            and hashes(path) == (asset["sha256"], asset["md5"]))


def fetch_asset(asset):
    relative = Path(asset["dgm"]) / asset["path"]
    destination = STATE / "source" / relative
    destination.parent.mkdir(parents=True, exist_ok=True)
    if valid(destination, asset):
        return {"file": str(relative), "status": "verified-existing"}
    local = ROOT / "resources" / relative
    discrepancy = None
    if valid(local, asset):
        shutil.copyfile(local, destination)
        return {"file": str(relative), "status": "verified-local"}
    if local.exists():
        discrepancy = {"bytes": local.stat().st_size, "sha256": hashes(local)[0]}
    temporary = destination.with_suffix(destination.suffix + ".part")
    for attempt in range(8):
        try:
            response = request(asset["url"], params={"version": asset["osf_version"]}, stream=True)
            with response, temporary.open("wb") as stream:
                for block in response.iter_content(4 * 1024 * 1024):
                    stream.write(block)
            if valid(temporary, asset):
                os.replace(temporary, destination)
                return {"file": str(relative), "status": "downloaded", "local_difference": discrepancy}
        except (requests.RequestException, OSError, RuntimeError):
            if attempt == 7:
                raise RuntimeError(f"Unable to obtain verified source file: {relative}") from None
        time.sleep(min(60, 2 ** attempt))
    raise RuntimeError(f"Source hash mismatch after eight attempts: {relative}")


def fetch():
    value = json.loads((STATE / "inventory.json").read_text(encoding="utf-8"))
    report = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        futures = {pool.submit(fetch_asset, asset): asset for asset in value["assets"]}
        for future in concurrent.futures.as_completed(futures):
            report.append(future.result())
            if len(report) % 25 == 0 or len(report) == len(futures):
                atomic_json(STATE / "source-verification.json", report)
                print(f"Verified {len(report)}/{len(futures)} source files", flush=True)
    atomic_json(STATE / "source-verification.json", report)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=["inventory", "fetch"])
    args = parser.parse_args()
    {"inventory": inventory, "fetch": fetch}[args.action]()
