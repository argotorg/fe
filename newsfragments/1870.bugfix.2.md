`Option::is_some`, `Option::is_none`, `Result::is_ok` and `Result::is_err` read the value instead of consuming it, so querying an `Option` or `Result` that owns a value leaves it usable.
