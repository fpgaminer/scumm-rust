Before finishing your work, please ensure that the code passes:
* `cargo +nightly fmt --all -- --check`
* `cargo check --all-features`
* `cargo test --all-features`
* `cargo clippy --all-features -- -D warnings`

All of these are required by the GitHub Actions workflow (`.github/workflows/test.yml`), which will cause your pull request to fail if any of these checks do not pass.