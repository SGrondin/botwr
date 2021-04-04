#! /usr/bin/env bash

set -euo pipefail

### Variables

set +u
if [ -z "$BOTWR_CREDENTIALS" ]; then
  echo 'The BOTWR_CREDENTIALS environment variable is missing from your .bashrc or .bash_profile. It should contain the path to your service account key file.'
  exit 1
fi
set -u

if ! ls "$BOTWR_CREDENTIALS" &> /dev/null; then
  echo "Could not find your GCloud credentials file. Check your BOTWR_CREDENTIALS variable: $BOTWR_CREDENTIALS"
  return 1
fi

export GOOGLE_APPLICATION_CREDENTIALS="$BOTWR_CREDENTIALS"
export GCLOUD_PROJECT="$BOTWR_PROJECT"

if which greadlink &> /dev/null; then
  export PROJECT_DIR="$(dirname "${BASH_SOURCE[0]}" | xargs greadlink -f)"
else
  export PROJECT_DIR="$(dirname "${BASH_SOURCE[0]}" | xargs readlink -f)"
fi

## Check if there uncommitted work
if ! (git diff --quiet && git diff --staged --quiet); then
  echo 'Please commit your work in progress before running this script.'
  exit 1
fi

### Authenticate

gcloud auth activate-service-account --key-file="$GOOGLE_APPLICATION_CREDENTIALS"
gcloud config set project "$GCLOUD_PROJECT"
gcloud config set run/region us-central1

### Build and upload

URL="us.gcr.io/$GCLOUD_PROJECT/botwr"
SHA="$(git rev-list -1 HEAD -- .)"
IMAGE="$URL:$SHA"
docker build . -t "$IMAGE"
docker push "$IMAGE"

### Deploy

gcloud run deploy botwr --platform=managed --image="$IMAGE"
