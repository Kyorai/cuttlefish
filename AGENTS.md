# Cuttlefish - AI Agent Reference

## What This Codebase Is

Cuttlefish is an Erlang library that translates a sysctl-style `.conf` file
into an Erlang `app.config` proplist. It is used as a build-time and
startup-time tool by Erlang/OTP applications (originally Riak, now RabbitMQ
and others). The primary artifact is an escript (`cuttlefish`) that is invoked
by shell scripts to generate `app.config` from `*.schema` files and a
`*.conf` file.

## Core Data Types

These types flow through almost every function in the codebase. Know them.

### `cuttlefish_variable:variable()`

A dot-separated config key, tokenized into a list of strings.

```
"ring_size"           -> ["ring_size"]
"log.error.file"      -> ["log", "error", "file"]
"listener.tcp.$name"  -> ["listener", "tcp", "$name"]  % fuzzy variable
```

`$`-prefixed segments are "fuzzy" - they act as wildcards matching any value.
`cuttlefish_variable` is the module for all variable manipulation:
`tokenize/1`, `format/1`, `is_fuzzy_match/2`, `fuzzy_matches/2`, etc.

### `cuttlefish_conf:conf()`

A proplist of `{variable(), string()}` pairs. Values are always raw strings
at this stage - datatype conversion happens later in the pipeline.

```erlang
[{["ring", "size"], "32"}, {["log", "error", "file"], "/var/log/error.log"}]
```

### `cuttlefish_schema:schema()`

A 3-tuple: `{[translation()], [mapping()], [validator()]}`.

### `cuttlefish_mapping:mapping()`

An opaque `#mapping{}` record. Access only via the exported accessor functions.
Key fields:
- `variable` - tokenized conf key, e.g. `["ring", "size"]`
- `mapping` - Erlang app.config path, e.g. `"riak_core.ring_creation_size"`
- `default` - default value (term, not string)
- `datatype` - list of `cuttlefish_datatypes:datatype()` atoms/tuples
- `aliases` - list of deprecated `variable()` keys that map to this one
- `hidden` - if true, omitted from generated `.conf`
- `level` - `basic | intermediate | advanced`

### `cuttlefish_error:error()` and `cuttlefish_error:errorlist()`

```erlang
-type error()     :: {'error', {atom(), term()}}.
-type errorlist() :: {'errorlist', [error()]}.
```

Errors are accumulated and returned, not thrown (except inside translation
functions where `throw({invalid, Reason})` and `throw(unset)` are the
intended mechanism). Use `cuttlefish_error:xlate/1` to get a human-readable
string from an error tuple.

## The Pipeline

`cuttlefish_generator:map/2` is the main entry point. It runs these stages
in order:

```
conf file(s)  -->  cuttlefish_conf:file/1       -- parse .conf into conf()
schema file(s) --> cuttlefish_schema:files/1    -- parse .schema into schema()
                                                   (includes alias validation)
                   map_add_defaults/3
                     resolve_aliases/2           -- rewrite alias keys to canonical
                     add_defaults/2              -- inject schema defaults
                   map_value_sub/3
                     value_sub/1                 -- expand $(key) substitutions
                   map_transform_datatypes/3
                     transform_datatypes/3       -- string -> typed Erlang terms
                   map_validate/3
                     run_validations/3           -- run validator funs
                   map_translate/3
                     run_translations/3          -- run translation funs
                                                --> [{app, [{key, value}]}]
```

`minimal_map/2` is a variant that only processes mappings whose canonical key
or an alias appears in the conf (no defaults injected for unset keys).

## Schema Files

Schema files are Erlang term files (parsed with `erl_scan`/`erl_parse`).
Three term types are recognized:

```erlang
{mapping, "conf.key", "app.erlang_key", [Options]}.
{translation, "app.erlang_key", fun(Conf) -> ... end}.
{validator, "name", "description", fun(Value) -> boolean() end}.
```

Schema files are loaded via `cuttlefish_schema:files/1` or `strings/1`.
Multiple schema files are merged: later files can override earlier ones using
the `merge` option in a mapping's proplist.

### Mapping Options (proplist)

| Key | Value | Notes |
|-----|-------|-------|
| `{datatype, T}` | datatype or list | see `cuttlefish_datatypes` |
| `{default, V}` | any term | used when key absent from conf |
| `{commented, V}` | any term | like default but commented out in generated conf |
| `{level, L}` | `basic\|intermediate\|advanced` | |
| `{validators, [Name]}` | list of validator names | |
| `{hidden, true}` | boolean | omit from generated conf |
| `{alias, "old.key"}` | string | single deprecated alias |
| `{aliases, ["old.key"]}` | list of strings | multiple deprecated aliases |
| `merge` | atom | merge with existing mapping instead of replacing |
| `{include_default, "name"}` | string | |
| `{see, ["other.key"]}` | list | cross-references |

## Alias System (added in 3.7.0)

Aliases allow a mapping to accept deprecated conf keys transparently.

```erlang
{mapping, "new.key", "app.setting", [
    {datatype, integer},
    {default, 42},
    {alias, "old.key"}   % or {aliases, ["old.key", "older.key"]}
]}.
```

**How it works:**

1. Schema validation (`cuttlefish_schema:validate_aliases/1`) runs at schema
   load time and rejects: alias shadowing a canonical key, alias claimed by
   multiple mappings.

2. `cuttlefish_generator:resolve_aliases/2` runs early in the pipeline
   (before `add_defaults`). It rewrites alias keys to their canonical key,
   logs a deprecation warning, and removes the alias entry from conf.
   Canonical key wins if both are set. First alias in declaration order wins
   if multiple aliases are set and canonical is absent.

3. Alias resolution is idempotent - a second pass is a no-op.

4. Aliases on fuzzy variables (e.g. `listener.tcp.$name`) are explicitly
   unsupported and rejected at parse time.

**Critical gotcha:** Alias keys are gone from conf before RHS substitution
runs. `$(old.key)` in a substitution will fail with a
`{substitution_alias_key, ...}` error naming the canonical key to use instead.
Use `$(new.key)`.

**Merge behavior:** `{aliases, []}` in a merge mapping explicitly clears
inherited aliases. This is intentionally asymmetric with `see` and `doc`,
where an empty list means "not specified, inherit".

## Module Reference

### `cuttlefish` (public API for schema writers)

- `conf_get/2,3` - get a value from conf by variable or dot-string
- `unset/0` - throw from a translation to omit the setting
- `invalid/1,2` - throw from a translation to report invalid input
- `warn/1,2` - log a warning from a translation
- `otp/2,3` - OTP version comparison helper

### `cuttlefish_schema`

Loads and merges schema files. `filter/1` runs after merging: removes
duplicate mappings to the same Erlang key when no translation exists (keeps
first), then runs `validate_aliases/1`.

### `cuttlefish_mapping`

Record model for a single mapping. `parse/1` validates and constructs a
`#mapping{}` from a raw `{mapping, ...}` tuple. Returns `{error, ...}` on
invalid input - callers must check.

`parse_and_merge/2` is the accumulator used when loading schema files. If
`merge` is in the proplist, it calls `merge/2` which calls `parse/1`
internally - both propagate errors.

`choose/4` implements merge field selection: `new` if the field is defined in
the merge proplist, `old` (inherit) otherwise. Special cases: empty `see`/`doc`
in a merge mapping means inherit; empty `aliases` means clear.

### `cuttlefish_generator`

The pipeline engine. Key exported functions:
- `map/2,3` - full pipeline
- `minimal_map/2` - pipeline without defaults for unset keys
- `add_defaults/2` - inject defaults (also used by `cuttlefish_effective`)
- `resolve_aliases/2` - alias rewriting (also used by `cuttlefish_effective`)
- `find_mapping/2` - find a mapping by variable, handles fuzzy matching

### `cuttlefish_conf`

Parses `.conf` files via `conf_parse` (PEG-generated parser). Handles
`@include` directives, BOM stripping, multiline values (`'''..'''`).
`generate/1` and `generate_file/2` produce `.conf` output from a schema.

### `cuttlefish_effective`

Generates the "effective configuration" display (what `cuttlefish effective`
prints). Runs `resolve_aliases` and `add_defaults` but not the full pipeline.

### `cuttlefish_advanced`

Merges an `advanced.config` proplist on top of a generated config. Simple
`lists:keystore` overlay - no schema awareness.

### `cuttlefish_datatypes`

String-to-typed-value conversion. Supports: `integer`, `string`, `atom`,
`float`, `bytesize`, `duration`, `ip`, `fqdn`, `flag`, `{enum, [...]}`,
`tagged_string`, `tagged_binary`, `{list, T}`, etc.

`from_string/2` is the main entry point. Returns `{error, {conversion, ...}}`
on failure.

### `cuttlefish_variable`

All variable manipulation. `tokenize/1` and `format/1` are inverses.
Dots can be escaped with `\.` to include a literal dot in a segment.

### `cuttlefish_error`

Error type definitions and `xlate/1` for human-readable messages. All error
atoms are defined here with their `xlate` clauses. When adding a new error
type, add both the atom usage and the `xlate` clause.

### `cuttlefish_validator`

Validator record model. Validators are named functions `fun(Value) -> boolean()`.
Referenced by name in mapping `validators` lists.

### `cuttlefish_translation`

Translation record model. Translations are `fun(Conf) -> term()` functions
that produce the final Erlang value for an app.config key. Called after
datatype conversion. `Conf` at this point is a typed proplist.

### `cuttlefish_escript`

The escript entry point (`main/1`). Parses CLI args with `getopt`, dispatches
to commands: `generate`, `effective`, `describe`, `print`, `version`.

### `cuttlefish_util`

Utility functions. `replace_proplist_value/3`, `numerify/1`, `levenshtein/2`,
`read_and_parse_file/2` (used by the PEG-generated parsers), BOM stripping.
The `conf_get_value`, `filter_by_variable_starts_with`, etc. exports are
deprecated wrappers - do not use them in new code.

### `cuttlefish_vmargs`

Converts a proplist to `vm.args` format strings. Minimal module.

### `conf_parse` / `conf_parse.peg`

PEG grammar (Neotoma) for `.conf` files. `conf_parse.erl` is generated - do
not edit directly. If regenerating, the `file/1` function must be manually
replaced as documented in `README.md` and the `.peg` file header.

### `cuttlefish_duration_parse` / `cuttlefish_duration_parse.peg`

PEG grammar for duration values (e.g. `5s`, `2h30m`). Same regeneration
caveat as `conf_parse`.

### `cuttlefish_rebar_plugin`

Legacy rebar2 plugin. Not used with rebar3. Excluded from Dialyzer analysis.
Do not modify.

### `cuttlefish_unit`

Test helper that renders schema documentation using `bbmustache`. Only
meaningful in test context. Excluded from Dialyzer analysis.

## Testing

Tests live in two places:
- Inline in `src/*.erl` inside `-ifdef(TEST)` blocks (EUnit)
- Standalone in `test/*.erl`

Run all tests: `rebar3 eunit`
Run one module: `rebar3 eunit --module cuttlefish_generator`

Property-based tests use PropEr (`proper`). They are in
`test/cuttlefish_alias_proper_tests.erl` and run as part of the normal EUnit
suite (each property is wrapped in a `?PROP(...)` EUnit test).

Test logging helpers: `cuttlefish_test_logging` captures log output during
tests. Call `set_up/0` then `bounce(Level)` before the code under test, then
`get_logs/0` to retrieve captured messages.

Integration tests in `test/cuttlefish_escript_integration_tests.erl` invoke
the escript binary directly and check stdout/stderr/exit codes.

## Known Patterns and Gotchas

**Error propagation in `parse_and_merge/2`:** `parse/1` returns
`{error, ...}` on invalid input. `parse_and_merge/2` and `merge/2` both
propagate these errors. Callers of `parse_and_merge/2` that accumulate
mappings in a foldl must check for error returns - the schema loader
(`cuttlefish_schema:string/2`) does this via `parse_schema/3`.

**Fuzzy mappings and `find_mapping/2`:** A conf key like
`["listener", "tcp", "internal"]` may match a fuzzy mapping variable
`["listener", "tcp", "$name"]`. `find_mapping/2` handles both exact and
fuzzy lookup. When working with mappings, always use `find_mapping/2` rather
than direct list search.

**`conf()` keys are always tokenized lists, never strings.** The conf
proplist uses `["ring", "size"]` as keys, not `"ring.size"`. Mixing these
up is a common source of `proplists:get_value` returning `undefined`.

**Translation functions receive typed conf.** By the time translations run,
values have been converted from strings to their declared datatypes. Use
`cuttlefish:conf_get/2,3` inside translations, not direct proplist access.

**`cuttlefish:unset()` and `cuttlefish:invalid/1` are throws.** They are
intended to be called inside translation funs and are caught by the pipeline.
Do not call them outside a translation context.

**`advanced.config` overlay happens after the full pipeline.** It is a raw
Erlang proplist merge with no schema validation. Settings in `advanced.config`
silently override generated values.

**The `merge` atom in a mapping proplist** means "merge with the existing
mapping of the same variable rather than replace it". It is used when multiple
schema files contribute to the same mapping (e.g. a base schema and an
overlay). The `choose/4` function in `cuttlefish_mapping` implements the
field-level merge logic.

**Dialyzer configuration:** `rebar.config` excludes `cuttlefish_rebar_plugin`
and `cuttlefish_unit` from analysis and adds `syntax_tools`, `eunit`, and
`getopt` to the PLT. Run `rebar3 dialyzer` to verify - it should produce zero
warnings.

## CI

`.github/workflows/ci.yml` runs the test suite on Erlang 26, 27, and 28,
on both `ubuntu-latest` and `windows-latest`. Dialyzer runs only on the
latest OTP release (28) on Ubuntu. The project targets these OTP versions
per `README.md`.

## Comments

 * Only add comments that explain non-obvious intent
 * Place comments above the line they refer to, not at the end of the line
 * Use proper English grammar with articles, punctuation, and full stops at the end of sentences except for Markdown list items

## Git Instructions

 * Never add yourself to the list of commit co-authors
 * Never mention yourself in commit messages in any way
 * Do not commit changes automatically without explicit permission

The main development branch is `main`. The upstream default branch is still
set to `develop` for historical reasons, but `develop` is stale (v2.x).

RabbitMQ pulls cuttlefish from [Hex](https://hex.pm/packages/cuttlefish).
The library is maintained by the RabbitMQ core team.

## Writing Style Guide

 * Never add full stops to Markdown list items
