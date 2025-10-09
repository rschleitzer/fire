# Debugging Guide for Fire FHIR Server

## Prerequisites

### Required VS Code Extension

Install the **CodeLLDB** extension for Rust debugging:

1. Open VS Code Command Palette (`Cmd+Shift+P` on macOS, `Ctrl+Shift+P` on Windows/Linux)
2. Type "Extensions: Install Extensions"
3. Search for "CodeLLDB" by vadimcn
4. Click Install

Alternatively, VS Code will prompt you to install recommended extensions when you open this project.

## Quick Start

### 1. Start the Database

Before debugging, make sure PostgreSQL is running:

```bash
docker compose up postgres -d
```

Or use the VS Code task: `Cmd+Shift+P` → "Tasks: Run Task" → "docker: start postgres"

### 2. Set Breakpoints

Click in the left gutter next to any line number to set a breakpoint (a red dot will appear).

### 3. Start Debugging

Press `F5` or click the debug icon in the sidebar and select a debug configuration.

## Debug Configurations

### 1. Debug executable 'fire' (Main Server)

**Use this to debug the running server.**

- **Shortcut**: Press `F5`
- **What it does**: Builds and runs the main Fire FHIR server with the debugger attached
- **Environment**: Uses local PostgreSQL database on port 5432
- **Log level**: Set to `debug` for detailed output

**How to use:**
1. Set breakpoints in your handler code (e.g., `src/api/handlers/patient.rs`)
2. Press `F5`
3. Server starts at http://127.0.0.1:3000
4. Make HTTP requests (using curl, Postman, or browser)
5. Execution will pause at your breakpoints

**Example workflow:**
```bash
# Set breakpoint in src/api/handlers/patient.rs at line 50
# Press F5 to start debugging
# In another terminal:
curl -X POST http://localhost:3000/fhir/Patient \
  -H "Content-Type: application/json" \
  -d '{"resourceType": "Patient", "name": [{"family": "Test"}]}'
# Debugger will pause at your breakpoint
```

### 2. Debug unit tests

**Use this to debug library tests.**

- **Configuration**: "Debug unit tests"
- **What it does**: Runs all unit tests in `src/` with debugger
- **Database**: Uses `fhir_test` database

**How to use:**
1. Set breakpoint in a test function (e.g., in `src/models/patient.rs`)
2. Select "Debug unit tests" from debug dropdown
3. Press `F5`

### 3. Debug specific test

**Use this to debug a single test by name.**

- **Configuration**: "Debug specific test"
- **What it does**: Prompts for test name and debugs only that test
- **Most useful for**: Debugging failing tests

**How to use:**
1. Set breakpoint in your test function
2. Select "Debug specific test"
3. Press `F5`
4. Enter test name when prompted (e.g., `test_create_patient`)

**Example:**
```rust
#[tokio::test]
async fn test_create_patient() {
    // Set breakpoint here
    let patient = Patient { ... };
    // Debugger will pause
}
```

### 4. Debug integration tests

**Use this to debug tests in `tests/` directory.**

- **Configuration**: "Debug integration tests"
- **What it does**: Prompts for test file name and debugs it

## Debugging Features

### Inspect Variables

When paused at a breakpoint:
- **Hover** over variables to see their values
- **Variables panel** (left sidebar) shows all local variables
- **Watch panel** lets you add expressions to monitor

### Step Through Code

Use the debug toolbar or keyboard shortcuts:
- **Continue** (`F5`): Resume execution until next breakpoint
- **Step Over** (`F10`): Execute current line, don't enter functions
- **Step Into** (`F11`): Enter into function calls
- **Step Out** (`Shift+F11`): Exit current function
- **Restart** (`Cmd+Shift+F5`): Restart debugging session
- **Stop** (`Shift+F5`): Stop debugging

### Debug Console

Use the Debug Console (bottom panel) to:
- Evaluate expressions while paused
- Print variable values: Type variable name and press Enter
- Execute code: Type any valid Rust expression

Example:
```
> patient.id
Some("550e8400-e29b-41d4-a716-446655440000")
> format!("{:?}", patient.name)
[Name { family: Some("Smith"), given: Some(["John"]) }]
```

### Conditional Breakpoints

Right-click a breakpoint to add conditions:
- **Expression**: Break only when expression is true (e.g., `patient.id.is_none()`)
- **Hit Count**: Break after N hits (e.g., break on 5th iteration of loop)
- **Log Message**: Log message without stopping execution

### Logpoints

Instead of `println!` debugging, use logpoints:
1. Right-click in gutter
2. Select "Add Logpoint"
3. Enter message with variables in curly braces: `Patient ID: {patient.id}`

## Command-Line Debugging

### With rust-lldb

```bash
# Build with debug symbols
cargo build

# Start debugger
rust-lldb ./target/debug/fire

# Set breakpoint
(lldb) breakpoint set --file patient.rs --line 50

# Run
(lldb) run

# When paused:
(lldb) print patient
(lldb) continue
```

### With GDB

```bash
# Build with debug symbols
cargo build

# Start debugger
rust-gdb ./target/debug/fire

# Set breakpoint
(gdb) break patient.rs:50

# Run
(gdb) run

# When paused:
(gdb) print patient
(gdb) continue
```

## Debugging Tips

### 1. Use Tracing for Complex Issues

Instead of setting many breakpoints, add tracing:

```rust
use tracing::{debug, info, error};

#[tracing::instrument(skip(pool))]
async fn create_patient(pool: &PgPool, patient: Patient) -> Result<Patient> {
    debug!("Creating patient with ID: {:?}", patient.id);

    let result = query!("INSERT INTO ...").execute(pool).await;

    match result {
        Ok(_) => info!("Patient created successfully"),
        Err(e) => error!("Failed to create patient: {}", e),
    }

    result
}
```

Run with: `RUST_LOG=fire=debug cargo run`

### 2. Debug Database Queries

Set breakpoint before query execution and inspect the SQL:

```rust
let query = sqlx::query_as!(Patient, "SELECT * FROM patient WHERE id = $1", id);
// Set breakpoint here to see the query and parameters
let patient = query.fetch_one(pool).await?;
```

### 3. Test in Isolation

For complex functions, write a test first:

```rust
#[tokio::test]
async fn test_search_patients_by_name() {
    let pool = setup_test_db().await;
    // Set breakpoint in function being tested
    let results = search_patients(&pool, "Smith").await;
    assert!(!results.is_empty());
}
```

### 4. Debug Async Code

Async code can be tricky. Use the debugger to:
- Inspect `Future` states
- Check if code is actually awaiting
- Verify task execution order

### 5. Inspect HTTP Requests/Responses

Set breakpoints in handlers to see:
- Incoming request body: `Json(patient): Json<Patient>`
- Parsed parameters: `Query(params): Query<SearchParams>`
- Response before sending: `let response = Json(patient);`

## Common Debugging Scenarios

### Server Not Starting

1. Set breakpoint in `main.rs` at `let app = Router::new()`
2. Check if database connection succeeds
3. Verify environment variables are loaded

### Query Returns Wrong Results

1. Set breakpoint before query execution
2. Inspect query parameters
3. Copy SQL to psql and run manually:
   ```bash
   psql -d fhir -U postgres
   SELECT * FROM patient WHERE name_family LIKE '%Smith%';
   ```

### Test Failing

1. Use "Debug specific test" configuration
2. Step through test line by line
3. Compare expected vs actual values in Variables panel

### Performance Issue

1. Add tracing with timestamps
2. Set breakpoints at function entry/exit
3. Check database query execution times:
   ```rust
   let start = std::time::Instant::now();
   let result = query.fetch_all(pool).await?;
   debug!("Query took {:?}", start.elapsed());
   ```

## VS Code Tasks

Run common tasks from Command Palette (`Cmd+Shift+P`):

- **rust: cargo build** - Build project
- **rust: cargo run** - Run server with debug logging
- **rust: cargo test** - Run all tests
- **rust: cargo check** - Quick syntax check
- **rust: cargo clippy** - Run linter
- **docker: start postgres** - Start database
- **docker: stop postgres** - Stop database

## Additional Resources

- [CodeLLDB Documentation](https://github.com/vadimcn/vscode-lldb/blob/master/MANUAL.md)
- [Rust Debugging Guide](https://doc.rust-lang.org/book/ch12-06-writing-to-stderr-instead-of-stdout.html)
- [Tokio Console](https://github.com/tokio-rs/console) - For advanced async debugging
- [tracing Documentation](https://docs.rs/tracing/latest/tracing/)

## Troubleshooting

### Breakpoints Not Working

- Ensure you're building in debug mode (default for `cargo build`)
- Check that breakpoints are in actually executed code
- Try cleaning and rebuilding: `cargo clean && cargo build`

### "Database connection refused"

- Start PostgreSQL: `docker compose up postgres -d`
- Verify DATABASE_URL in `.env` is correct
- Check database is accepting connections: `docker exec fire-postgres pg_isready`

### CodeLLDB Not Working

- Reinstall extension: Uninstall → Restart VS Code → Install
- Check LLDB is installed: `lldb --version`
- On macOS, you might need Xcode Command Line Tools: `xcode-select --install`

### Can't See Variable Values

- Variable might be optimized away in release builds
- Use debug build: `cargo build` (not `cargo build --release`)
- Try moving variable to where you can inspect it

### Debugging is Slow

- Limit breakpoints to critical sections
- Use conditional breakpoints instead of breaking every time
- Consider using logpoints and tracing instead
