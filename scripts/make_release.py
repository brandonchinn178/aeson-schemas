# pyright: strict, reportUnknownMemberType=false

from __future__ import annotations

import itertools
import json
import logging
import os
import re
import requests
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.DEBUG)


def main():
    gh_token = os.environ["gh_token"]
    hackage_token = os.environ["hackage_token"]
    version = os.environ["version"]
    sdistdir = os.environ["sdistdir"]
    repo = os.environ["GITHUB_REPOSITORY"]
    sha = os.environ["GITHUB_SHA"]

    version_name = f"v{version}"

    # check inputs
    if not hackage_token:
        raise Exception(
            "Hackage token is not provided (did you add a Secret of the form HACKAGE_TOKEN_<github username>?)"
        )

    # ensure release files exist
    sdist_archive = Path(sdistdir) / f"aeson-schemas-{version}.tar.gz"
    if not sdist_archive.exists():
        raise Exception(f"File does not exist: {sdist_archive}")

    logger.info(f"Creating release {version_name}")

    # check + parse CHANGELOG
    changelog = Path("CHANGELOG.md").read_text()
    changelog = re.sub(r"^# Unreleased\n+", "", changelog)
    if not changelog.startswith(f"# {version_name}"):
        raise Exception("CHANGELOG doesn't look updated")
    version_changes = get_version_changes(changelog)

    create_github_release(
        repo=repo,
        token=gh_token,
        sha=sha,
        version_name=version_name,
        version_changes=version_changes,
    )

    # uploading as candidate because uploads are irreversible, unlike
    # GitHub releases, so just to be extra sure, we'll upload this as
    # a candidate and manually confirm uploading the package on Hackage
    upload_hackage_candidate(
        token=hackage_token,
        archive=sdist_archive,
    )

    logger.info(f"Released aeson-schemas {version_name}!")


def get_version_changes(changelog: str) -> str:
    lines = changelog.split("\n")

    # skip initial '# vX.Y.Z' line
    lines = lines[1:]

    # take lines until the next '# vX.Y.Z' line
    lines = itertools.takewhile(lambda line: not line.startswith("# v"), lines)

    return "\n".join(lines)


def create_github_release(
    *,
    repo: str,
    token: str,
    sha: str,
    version_name: str,
    version_changes: str,
):
    session = init_session()
    session.headers["Accept"] = "application/vnd.github.v3+json"
    session.headers["Authorization"] = f"token {token}"
    session.headers["User-Agent"] = repo

    payload = {
        "tag_name": version_name,
        "target_commitish": sha,
        "name": version_name,
        "body": version_changes,
    }
    logger.debug(f"Creating release with: {json.dumps(payload)}")

    session.post(
        f"https://api.github.com/repos/{repo}/releases",
        json=payload,
    )


def upload_hackage_candidate(
    *,
    token: str,
    archive: Path,
):
    session = init_session()
    with archive.open("rb") as f:
        session.post(
            "https://hackage.haskell.org/packages/candidates",
            headers={"Authorization": f"X-ApiKey {token}"},
            files={"package": f},
        )


def init_session() -> requests.Session:
    session = requests.Session()

    def _check_status(r: requests.Response, *args: Any, **kwargs: Any):
        r.raise_for_status()

    # https://github.com/python/typeshed/issues/7776
    session.hooks["response"].append(  # pyright: ignore[reportFunctionMemberAccess]
        _check_status,
    )

    return session


if __name__ == "__main__":
    main()
