#!/usr/bin/env bash
#
# Release script for dungeon-haskell.
#
#   usage: scripts/release.sh VERSION
#
#   VERSION is a bare semver string like 0.1.0 (no leading "v"). The
#   script tags the current HEAD as vVERSION, builds a portable
#   linux/amd64 binary inside a Debian Docker container, bundles it
#   with the game assets and a launcher script, and attaches the
#   tarball to a GitHub release.
#
# Why Docker:
#
#   The dev host is NixOS, and any binary produced there links
#   against /nix/store paths that don't exist on other systems. We
#   run the build inside `scripts/Dockerfile.build` (Debian Bookworm +
#   GHC 9.10.3 + libSDL2) so the resulting binary is glibc-linked and
#   portable to any modern Linux distro with libSDL2 installed.
#
# Flow:
#
#   1. sanity checks (clean tree, tag free, gh/docker available)
#   2. build the Docker image (cached between runs)
#   3. run the full test suite inside the container
#   4. build the release binary inside the container
#   5. stage the binary + assets + launcher into a tarball
#   6. CONFIRM with the user before doing anything destructive
#   7. tag vVERSION + push to origin
#   8. `gh release create` and upload the tarball
#
# Safety:
#
#   Nothing that touches the world (git push, gh release) happens
#   before the interactive confirmation in step 6. You can Ctrl-C
#   any time before that prompt and the only thing left behind is
#   the staged tarball in the current directory.

set -euo pipefail

PROJECT="dungeon-haskell"
IMAGE="${PROJECT}-build:latest"
DOCKERFILE="scripts/Dockerfile.build"
STACK_WORK_DOCKER=".stack-work-docker"
STACK_CACHE_VOLUME="dh-stack-cache"

#--------------------------------------------------------------
# argument parsing
#--------------------------------------------------------------

VERSION="${1:-}"
if [[ -z "$VERSION" ]]; then
  echo "usage: $0 VERSION (e.g. 0.1.0)" >&2
  exit 2
fi

if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "error: VERSION must look like MAJOR.MINOR.PATCH (got: $VERSION)" >&2
  exit 2
fi

TAG="v${VERSION}"
ARTIFACT="${PROJECT}-${VERSION}-linux-x86_64.tar.gz"
BUNDLE_NAME="${PROJECT}-${VERSION}"

#--------------------------------------------------------------
# sanity checks
#--------------------------------------------------------------

if [[ ! -f "dungeon-haskell.cabal" ]]; then
  echo "error: run this script from the project root" >&2
  exit 1
fi

if ! git diff-index --quiet HEAD --; then
  echo "error: working tree has uncommitted changes; commit or stash first" >&2
  exit 1
fi

if git rev-parse "$TAG" >/dev/null 2>&1; then
  echo "error: tag $TAG already exists locally" >&2
  exit 1
fi

for tool in docker gh git tar; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "error: required tool \"$tool\" not found in PATH" >&2
    exit 1
  fi
done

if ! gh auth status >/dev/null 2>&1; then
  echo "error: gh is not authenticated; run \`gh auth login\` first" >&2
  exit 1
fi

#--------------------------------------------------------------
# docker image
#--------------------------------------------------------------

echo "==> building Docker image: $IMAGE"
docker build -f "$DOCKERFILE" -t "$IMAGE" scripts/

#--------------------------------------------------------------
# build inside the container
#
# Notes:
#
#   * The project is bind-mounted read-write into /workspace so the
#     build sees the exact source tree at HEAD. A separate
#     $STACK_WORK_DOCKER work dir keeps Debian-built artifacts out of
#     the host's .stack-work (which may have been populated by a
#     NixOS build and has an incompatible GHC path layout).
#
#   * A named docker volume caches ~/.stack between runs so the
#     snapshot download + dep compile only happens on the first
#     release cut.
#
#   * Build output is copied to a predictable path under the mount
#     (./.release-bin/$PROJECT) so the host can pick it up without
#     having to parse `stack path` output.
#--------------------------------------------------------------

# Clean any leftover bin from a previous aborted run so we don't
# accidentally ship a stale binary if the build inside the container
# fails silently.
rm -rf .release-bin
mkdir -p .release-bin

echo "==> running test suite inside container"
docker run --rm \
  -v "$PWD:/workspace" \
  -v "$STACK_CACHE_VOLUME:/root/.stack" \
  -w /workspace \
  "$IMAGE" \
  stack --work-dir "$STACK_WORK_DOCKER" --system-ghc --no-nix test --fast

echo "==> building release binary inside container"
docker run --rm \
  -v "$PWD:/workspace" \
  -v "$STACK_CACHE_VOLUME:/root/.stack" \
  -w /workspace \
  "$IMAGE" \
  bash -c "
    set -euo pipefail
    stack --work-dir $STACK_WORK_DOCKER --system-ghc --no-nix build
    bin=\"\$(stack --work-dir $STACK_WORK_DOCKER --system-ghc --no-nix path --local-install-root)/bin/$PROJECT\"
    cp \"\$bin\" /workspace/.release-bin/$PROJECT
  "

if [[ ! -x ".release-bin/$PROJECT" ]]; then
  echo "error: build did not produce .release-bin/$PROJECT" >&2
  exit 1
fi

#--------------------------------------------------------------
# stage the tarball
#--------------------------------------------------------------

STAGE="$(mktemp -d)"
trap 'rm -rf "$STAGE" .release-bin' EXIT

BUNDLE="$STAGE/$BUNDLE_NAME"
mkdir -p "$BUNDLE"

cp ".release-bin/$PROJECT" "$BUNDLE/$PROJECT"
chmod +x "$BUNDLE/$PROJECT"

# `strip` is best-effort. It shrinks the binary considerably
# but isn't strictly necessary — if it fails (e.g. strip not
# installed) the binary still works.
if command -v strip >/dev/null 2>&1; then
  strip "$BUNDLE/$PROJECT" || true
fi

# Ship assets next to the binary so audio loads correctly.
cp -r assets "$BUNDLE/"

# Ship the user-facing docs next to the binary.
cp README.md LICENSE "$BUNDLE/"

# Launcher that cd's into its own directory so the relative
# `assets/` paths resolve regardless of where the user runs the
# game from.
cat > "$BUNDLE/run.sh" <<'WRAPPER'
#!/usr/bin/env bash
# Thin launcher for dungeon-haskell. The game loads audio assets
# from ./assets/ relative to the current working directory, so we
# must cd into the install dir before exec'ing the binary.
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$here"
exec ./dungeon-haskell "$@"
WRAPPER
chmod +x "$BUNDLE/run.sh"

tar -czf "$ARTIFACT" -C "$STAGE" "$BUNDLE_NAME"
echo "==> built $ARTIFACT ($(du -h "$ARTIFACT" | cut -f1))"

#--------------------------------------------------------------
# confirmation gate
#
# Everything below this point is destructive / world-visible
# (pushes a tag, creates a GitHub release). Pause here so the
# caller can bail out cleanly after reviewing the tarball.
#--------------------------------------------------------------

echo
echo "About to:"
echo "  1. create git tag $TAG on $(git rev-parse --short HEAD)"
echo "  2. push $TAG to origin"
echo "  3. create GitHub release $TAG with $ARTIFACT attached"
echo
read -r -p "Proceed? [y/N] " ans
if [[ ! "$ans" =~ ^[Yy]$ ]]; then
  echo "aborted. $ARTIFACT is still in the current directory."
  # Disable the trap so we don't nuke the tarball on exit; the
  # .release-bin scratch dir can still go.
  trap 'rm -rf "$STAGE" .release-bin' EXIT
  exit 0
fi

#--------------------------------------------------------------
# tag + push + release
#--------------------------------------------------------------

echo "==> tagging $TAG"
git tag -a "$TAG" -m "Release $TAG"

echo "==> pushing tag to origin"
git push origin "$TAG"

echo "==> creating GitHub release"
gh release create "$TAG" "$ARTIFACT" \
  --title "$TAG" \
  --notes "Linux amd64 build of dungeon-haskell $VERSION.

**Install:**
\`\`\`
tar -xzf ${ARTIFACT}
cd ${BUNDLE_NAME}
./run.sh
\`\`\`

**Runtime dependency:** \`libSDL2\` must be installed for audio
playback. On Debian/Ubuntu: \`sudo apt install libsdl2-2.0-0\`. On
Fedora: \`sudo dnf install SDL2\`. On Arch: \`sudo pacman -S sdl2\`.
The game runs silently without it.

Built from \`$(git rev-parse --short HEAD)\` inside a Debian Bookworm
container for glibc portability."

echo
echo "==> done. Release $TAG is live."
