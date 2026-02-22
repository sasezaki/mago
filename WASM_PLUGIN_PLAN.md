# WASM-Based Plugin System for Mago: Implementation Plan

## Executive Summary

The existing plugin system in `mago-analyzer` is a clean, trait-based `Box<dyn T>` architecture
with a `PluginRegistry` that dispatches to ~15 distinct hook and provider traits. Adding WASM
plugins means building a host-side adapter (`WasmPlugin`) that implements `Plugin` (and registers
concrete `WasmFunctionReturnTypeAdapter`, `WasmHook*`, etc. structs) by translating Rust types
through a WASM boundary via JSON serialization. The recommended WASM runtime is **wasmtime**,
loaded as a new crate `mago-plugin-wasm`.

---

## 1. Architecture Overview

### New Crate: `crates/plugin-wasm/`

```
crates/
  plugin-wasm/
    Cargo.toml
    src/
      lib.rs             -- public API: load_wasm_plugin(path) -> Result<Box<dyn Plugin>>
      plugin.rs          -- WasmPlugin struct implementing Plugin trait
      adapter/
        mod.rs
        function_provider.rs   -- WasmFunctionReturnTypeAdapter
        method_provider.rs
        hook_program.rs
        hook_function_call.rs
        hook_method_call.rs
        hook_declaration.rs
        hook_issue_filter.rs
      abi/
        mod.rs
        types.rs         -- WasmTUnion, WasmInvocationInfo, WasmHookContext, WasmIssue, etc.
        host_funcs.rs    -- host functions exposed to WASM guests via Linker
      error.rs           -- WasmPluginError
```

A companion **SDK crate** for plugin authors:

```
crates/plugin-sdk/
  Cargo.toml
  src/
    lib.rs
    context.rs           -- GuestProviderContext, GuestHookContext
    types.rs             -- re-exports TUnion, WasmSpan, etc.
    macros.rs            -- declare_plugin! macro
    memory.rs            -- malloc/free for WASM linear memory
```

### WASM Runtime: wasmtime

**Decision: `wasmtime` (Bytecode Alliance)**

Rationale:
- First-class Rust support and Component Model support (future-proof)
- Excellent sandboxing (WASI selectively enabled)
- Production-grade (used by Fastly, Microsoft, AWS)
- Clean `wasmtime::Linker` API for host function injection
- Near-native performance via Cranelift JIT

```toml
# crates/plugin-wasm/Cargo.toml
[dependencies]
mago-analyzer = { workspace = true }
mago-reporting = { workspace = true }
mago-span      = { workspace = true }
mago-codex     = { workspace = true }
wasmtime       = { version = "25", default-features = false, features = ["cranelift"] }
serde          = { workspace = true }
serde_json     = { workspace = true }
tracing        = { workspace = true }
thiserror      = "2"
```

---

## 2. Host ↔ Guest ABI

### Host Functions (imported by WASM plugin)

```
mago::log(ptr: i32, len: i32)
    Write a UTF-8 log message.

mago::report_issue(json_ptr: i32, json_len: i32)
    Report a WasmReportedIssue (JSON) back to host.

mago::get_expression_type(span_json_ptr: i32, len: i32) -> i64
    Query type of expression by WasmSpan JSON.
    Returns (ptr << 32 | len) pointing to TUnion JSON. -1 if not found.

mago::get_variable_type(name_ptr: i32, name_len: i32) -> i64
    Returns type of a named variable.

mago::is_instance_of(class_ptr: i32, class_len: i32,
                      parent_ptr: i32, parent_len: i32) -> i32
    Returns 1 if class is instance of parent.

mago::set_expression_type(json_ptr: i32, json_len: i32)
    Set expression type (mutable hook context only).

mago::set_variable_type(json_ptr: i32, json_len: i32)
    Set variable type (mutable hook context only).
```

### Guest Exports (required from WASM plugin)

```wasm
;; Metadata
(export "mago_plugin_id"              (func)) ;; -> i64 (ptr<<32|len) to UTF-8 string
(export "mago_plugin_name"            (func)) ;; -> i64
(export "mago_plugin_description"     (func)) ;; -> i64
(export "mago_plugin_default_enabled" (func)) ;; -> i32 (0 or 1)

;; Lifecycle
(export "mago_plugin_init"    (func)) ;; -> i32 (0=ok, non-zero=error)
(export "mago_plugin_destroy" (func)) ;; cleanup

;; Registration manifest
(export "mago_get_manifest" (func))   ;; -> i64 pointing to WasmRegistrationManifest JSON

;; Providers
(export "mago_function_return_type"
        (func (param ptr i32) (param len i32) (result i64)))
;; param: JSON WasmFunctionCallContext; result: JSON WasmProviderResult

(export "mago_method_return_type"
        (func (param ptr i32) (param len i32) (result i64)))

;; Hooks
(export "mago_before_program"
        (func (param ptr i32) (param len i32) (result i32))) ;; 0=Continue, 1=Skip
(export "mago_after_program"
        (func (param ptr i32) (param len i32)))
(export "mago_before_function_call"
        (func (param ptr i32) (param len i32) (result i64)))
(export "mago_after_function_call"
        (func (param ptr i32) (param len i32)))
;; ... similarly for method_call, static_method_call, declarations, etc.

;; Memory management (WASM-side allocator)
(export "malloc" (func (param i32) (result i32)))
(export "free"   (func (param i32) (param i32)))
```

### Registration Manifest (JSON)

Returned by `mago_get_manifest` — declares what the plugin implements:

```json
{
  "function_providers": [
    { "kind": "exact",     "name": "my_custom_assert" },
    { "kind": "namespace", "name": "MyLib\\" }
  ],
  "method_providers": [
    { "class": "MyClass", "method": "getValue" }
  ],
  "hooks": ["before_program", "after_function_call", "issue_filter"]
}
```

---

## 3. Serialization Format

**JSON (via `serde_json`)** for V1.

Rationale:
- `TUnion` already derives `Serialize`/`Deserialize` in `mago-codex` — no custom serialization needed.
- `serde_json` is already a workspace dependency.
- Human-readable for debugging.
- Supported in all WASM-targetable languages (Rust, Go/TinyGo, C, AssemblyScript, Zig).

Performance estimate: ~1–10µs per boundary crossing. Acceptable for per-file and per-call hooks;
avoid for per-expression hooks (see scope limitation below).

**V2 option**: MessagePack (`rmp-serde`) as an optional feature flag.

---

## 4. Key ABI Types

```rust
// crates/plugin-wasm/src/abi/types.rs

#[derive(Serialize, Deserialize)]
pub struct WasmInvocationInfo {
    pub function_name: String,
    pub argument_count: usize,
    pub span: WasmSpan,
    pub arguments: Vec<WasmArgument>,
}

#[derive(Serialize, Deserialize)]
pub struct WasmArgument {
    pub index: usize,
    pub name: Option<String>,
    pub type_: Option<TUnion>,  // TUnion already serde-derives
    pub span: WasmSpan,
}

#[derive(Serialize, Deserialize)]
pub struct WasmSpan {
    pub file_id: u32,
    pub start_offset: u32,
    pub end_offset: u32,
}

#[derive(Serialize, Deserialize)]
pub struct WasmReportedIssue {
    pub code: String,
    pub level: WasmIssueLevel,
    pub message: String,
    pub primary_span: WasmSpan,
    pub secondary_spans: Vec<(WasmSpan, Option<String>)>,
    pub notes: Vec<String>,
    pub help: Option<String>,
    pub link: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub enum WasmIssueLevel { Error, Warning, Note, Help }

#[derive(Serialize, Deserialize)]
pub struct WasmProviderResult {
    pub return_type: Option<TUnion>,  // None = "no opinion"
    pub issues: Vec<WasmReportedIssue>,
}

#[derive(Serialize, Deserialize)]
pub struct WasmRegistrationManifest {
    pub function_providers: Vec<WasmFunctionProviderTarget>,
    pub method_providers: Vec<WasmMethodProviderTarget>,
    pub hooks: Vec<String>,
}
```

---

## 5. WasmPlugin Core Structure

```rust
// crates/plugin-wasm/src/plugin.rs

pub struct WasmPlugin {
    meta: WasmPluginMeta,
    manifest: WasmRegistrationManifest,
    instance: Arc<WasmSharedInstance>,
}

pub struct WasmSharedInstance {
    engine: Engine,   // Send + Sync in wasmtime
    module: Module,   // Send + Sync in wasmtime
    // Per-thread stores created lazily (see thread_local optimization below)
}

pub struct WasmHostState {
    pending_issues: Vec<WasmReportedIssue>,
    expression_type_query:  Option<Box<dyn Fn(WasmSpan) -> Option<TUnion> + Send>>,
    variable_type_query:    Option<Box<dyn Fn(String) -> Option<TUnion> + Send>>,
    is_instance_of_query:   Option<Box<dyn Fn(String, String) -> bool + Send>>,
    set_expression_type_cb: Option<Box<dyn FnMut(WasmSpan, TUnion) + Send>>,
    set_variable_type_cb:   Option<Box<dyn FnMut(String, TUnion) + Send>>,
}
```

**Thread-safety optimization**: Instead of `Mutex<(Store, Instance)>`, use `thread_local!`:

```rust
thread_local! {
    static WASM_STORES: RefCell<HashMap<PluginId, (Store<WasmHostState>, Instance)>>
        = RefCell::new(HashMap::new());
}
```

Each rayon worker thread creates its own `Store` + `Instance` on first use, eliminating mutex
contention across parallel file analysis.

**PluginMeta workaround**: `PluginMeta` fields are `&'static str`. Since WASM metadata is
runtime-dynamic, use `OnceLock` + `Box::leak`:

```rust
pub struct WasmPluginMeta {
    id: String,
    name: String,
    description: String,
    default_enabled: bool,
    static_meta: OnceLock<&'static PluginMeta>,
}
impl WasmPluginMeta {
    pub fn as_static(&self) -> &'static PluginMeta {
        self.static_meta.get_or_init(|| {
            Box::leak(Box::new(PluginMeta {
                id: Box::leak(self.id.clone().into_boxed_str()),
                name: Box::leak(self.name.clone().into_boxed_str()),
                description: Box::leak(self.description.clone().into_boxed_str()),
                aliases: &[],
                default_enabled: self.default_enabled,
            }))
        })
    }
}
```

---

## 6. PluginRegistry Changes

Add a dynamic registration path to `crates/analyzer/src/plugin/registry.rs`:

```rust
/// Object-safe version of FunctionReturnTypeProvider for runtime targets.
pub trait FunctionReturnTypeProviderDyn: Send + Sync {
    fn get_return_type_dyn(
        &self,
        context: &ProviderContext<'_, '_, '_>,
        invocation: &InvocationInfo<'_, '_, '_>,
    ) -> (Option<TUnion>, Vec<ReportedIssue>);
}

impl PluginRegistry {
    pub fn register_dynamic_function_provider(
        &mut self,
        target: FunctionTarget,
        provider: Box<dyn FunctionReturnTypeProviderDyn>,
    ) {
        let index = self.function_dynamic_providers.len();
        match &target {
            FunctionTarget::Exact(name) => {
                self.function_exact
                    .entry(ascii_lowercase_atom(name))
                    .or_default()
                    .push(index);
            }
            FunctionTarget::Prefix(prefix) => {
                self.function_prefix.push((ascii_lowercase_atom(prefix), index));
            }
            FunctionTarget::Namespace(ns) => {
                self.function_namespace.push((ascii_lowercase_atom(ns), index));
            }
            _ => {}
        }
        self.function_dynamic_providers.push(provider);
    }
}
```

Add `function_dynamic_providers: Vec<Box<dyn FunctionReturnTypeProviderDyn>>` to `PluginRegistry`.
Update dispatch in `get_function_return_type()` to also check `function_dynamic_providers`.

---

## 7. Data Flow: Crossing the WASM Boundary

The core pattern is **capture-by-callback + serialized snapshot**:

```
Before WASM call:
  1. Serialize call context → JSON (WasmInvocationInfo, WasmProviderContext)
  2. Install query callbacks in WasmHostState closing over ProviderContext refs
  3. Write JSON to WASM linear memory via guest malloc
  4. Call WASM function

During WASM execution:
  ↕  Guest calls mago::get_expression_type(span_json)
  ↕  Host callback fires, returns TUnion JSON
  ↕  Guest calls mago::report_issue(issue_json)
  ↕  Host appends to pending_issues

After WASM call:
  5. Read result JSON from WASM memory
  6. Clear query callbacks
  7. Drain pending_issues → convert to ReportedIssue
  8. Return (Option<TUnion>, Vec<ReportedIssue>)
```

Safety invariant: borrowed context references (passed as raw pointers to closures) are valid
only while the WASM call is executing on the same thread. Wasmtime synchronous calls are blocking
and single-threaded within a `Store`, so this is upheld.

---

## 8. Configuration Schema

`crates/src/config/analyzer.rs`:

```rust
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, JsonSchema)]
#[serde(default, rename_all = "kebab-case", deny_unknown_fields)]
pub struct WasmPluginEntry {
    /// Path to the .wasm file (relative to workspace root or absolute).
    pub path: String,
    pub enabled: Option<bool>,
}
```

`mago.toml` usage:

```toml
[analyzer]
plugins = ["stdlib"]

wasm-plugins = [
    { path = "plugins/my-custom-checks.wasm" },
    { path = "/absolute/path/to/framework-plugin.wasm", enabled = true },
]
```

---

## 9. Orchestrator Integration

`crates/orchestrator/src/lib.rs` — modify `get_analyzer_plugin_registry()`:

```rust
pub fn get_analyzer_plugin_registry(&self) -> Arc<PluginRegistry> {
    Arc::clone(self.plugin_registry.get_or_init(|| {
        let mut registry = create_registry_with_plugins(
            &self.config.analyzer_plugins,
            self.config.disable_default_analyzer_plugins,
        );

        // Load WASM plugins
        for entry in &self.config.wasm_plugin_paths {
            if entry.enabled.unwrap_or(true) {
                match mago_plugin_wasm::load_wasm_plugin(Path::new(&entry.path)) {
                    Ok(plugin) => {
                        tracing::info!("Loaded WASM plugin: {}", entry.path);
                        plugin.register(&mut registry);
                    }
                    Err(e) => {
                        tracing::error!("Failed to load WASM plugin {}: {}", entry.path, e);
                    }
                }
            }
        }

        Arc::new(registry)
    }))
}
```

---

## 10. Files to Modify

| File | Change |
|------|--------|
| `Cargo.toml` | Add `mago-plugin-wasm` to workspace members/deps |
| `crates/analyzer/src/plugin/registry.rs` | Add `register_dynamic_function_provider`, `function_dynamic_providers` field, `FunctionReturnTypeProviderDyn` trait |
| `crates/orchestrator/src/lib.rs` | Wire WASM plugin loading in `get_analyzer_plugin_registry()` |
| `crates/orchestrator/src/config.rs` | Add `wasm_plugin_paths: Vec<WasmPluginEntry>` |
| `src/config/analyzer.rs` | Add `WasmPluginEntry` struct and `wasm_plugins` field |
| `src/utils/mod.rs` | Pass `wasm_plugins` into `OrchestratorConfiguration` |

---

## 11. Guest SDK (mago-plugin-sdk)

Plugin authors write Rust compiled to `wasm32-unknown-unknown`:

```rust
use mago_plugin_sdk::prelude::*;

declare_plugin! {
    id: "my-company/custom-checks",
    name: "My Custom Checks",
    description: "Custom type rules for our codebase",
    default_enabled: true,
    function_providers: [
        FunctionTarget::Exact("my_assert_valid") => my_assert_provider,
    ],
    hooks: [before_program => my_before_program],
}

fn my_assert_provider(ctx: &GuestProviderContext, invoc: &GuestInvocationInfo) -> Option<TUnion> {
    let arg_type = invoc.argument_type(0)?;
    if arg_type.is_nullable() {
        ctx.report_issue(IssueBuilder::new("my-plugin::nullable-arg")
            .message("my_assert_valid does not accept nullable argument")
            .span(invoc.span())
            .build());
    }
    Some(TUnion::void())
}
```

**Languages that can write WASM plugins:**
- Rust (using `mago-plugin-sdk`, compiled to `wasm32-unknown-unknown`)
- Go (via TinyGo, manual JSON ABI)
- C/C++ (via wasi-sdk)
- AssemblyScript (TypeScript-like)
- Zig (native WASM support)

---

## 12. Security Sandbox

WASM plugins are sandboxed by default:
- **No filesystem access** (WASI not enabled)
- **No network access**
- **No arbitrary system calls**
- **Memory isolation** from host process
- **CPU time limiting** via wasmtime fuel:

```rust
store.add_fuel(10_000_000)?;  // 10M operations max per plugin call
```

---

## 13. Performance Considerations

| Factor | Impact | Mitigation |
|--------|--------|------------|
| wasmtime JIT startup | ~10–50ms per plugin load | Load once at startup |
| JSON serialization | ~1–10µs per call | Acceptable for per-file/per-call hooks |
| WASM memory copy | ~1µs per JSON write | Unavoidable; use lightweight context |
| Mutex per call | ~100ns contention | Use `thread_local!` stores per rayon worker |

---

## 14. V1 Scope (Pragmatic Subset)

| Feature | V1 | V2 |
|---------|----|----|
| Function return type providers (Exact) | ✅ | + Prefix/Namespace |
| Method return type providers | ❌ | ✅ |
| `before_program` / `after_program` hooks | ✅ | |
| `before_function_call` / `after_function_call` hooks | ✅ | |
| `issue_filter` hook | ✅ | |
| Class/interface/enum declaration hooks | ❌ | ✅ |
| Expression hooks (per expression) | ❌ | ✅ (opt-in) |
| Throw/assertion providers | ❌ | ✅ |
| MessagePack serialization option | ❌ | ✅ |

---

## 15. Implementation Phases

1. **Phase 1** — Foundation: Create `mago-plugin-wasm` crate, wasmtime engine setup, host function linking, `load_wasm_plugin()`, `WasmPlugin` implementing `Plugin`.

2. **Phase 2** — Registry: Add `register_dynamic_function_provider` to `PluginRegistry`. Implement `WasmFunctionReturnTypeAdapter`. Wire into Orchestrator.

3. **Phase 3** — Config + CLI: Add `wasm-plugins` to `AnalyzerConfiguration`, `OrchestratorConfiguration`. JSON schema support.

4. **Phase 4** — SDK: Create `mago-plugin-sdk` as a publishable crate. Write a sample plugin.

5. **Phase 5** — Optimization: thread-local stores, fuel limiting, MessagePack option.

---

## 16. Critical File Locations

- `crates/analyzer/src/plugin/registry.rs` — Core to modify for dynamic provider registration
- `crates/analyzer/src/plugin/plugin.rs` — `Plugin` trait that `WasmPlugin` must implement
- `crates/analyzer/src/plugin/context.rs` — `ProviderContext`/`HookContext` structure for boundary design
- `crates/orchestrator/src/lib.rs` — Integration point for WASM plugin loading
- `src/config/analyzer.rs` — Config struct patterns to follow for `WasmPluginEntry`
- `crates/codex/src/ttype/union.rs` — Confirms `TUnion: Serialize + Deserialize` (key ABI enabler)
