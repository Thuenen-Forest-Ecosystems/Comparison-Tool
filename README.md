# Comparison-Tool

R script for comparing tree, deadwood, regeneration, structure_gt4m and structure_lt4m across repeated forest inventory surveys

## Setup

Copy `.env.example` to `.env` and fill in your tfm-api credentials:

```dotenv
TFM_API_URL=https://ci.thuenen.de/
TFM_API_KEY=
TFM_API_EMAIL=
TFM_API_PASSWORD=
```

`TFM_API_KEY` is the public Supabase anon key for the tfm-api instance. `TFM_API_EMAIL`/`TFM_API_PASSWORD` are your personal login credentials.

In `Test_JSON.R`, set `cluster_name` and `plot_name` for the record you want to compare, then run the script. It logs in to tfm-api, downloads the matching record, and compares its current (`properties`) against the most recent historical version (`previous_properties`).
