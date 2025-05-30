Before finishing your work, please ensure that the code is properly formatted by running `cargo +nightly fmt`.
Make sure there are no compilation errors by running `cargo check --all-features`.
Make sure all tests pass by running `cargo test --all-features`.
Make sure clippy is happy: `cargo clippy --all-features -- -D warnings`.

All of these are required by the GitHub Actions workflow (`.github/workflows/test.yml`), which will cause your pull request to fail if any of these checks do not pass.