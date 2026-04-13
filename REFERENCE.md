# Cuttlefish Reference

This document is the comprehensive reference for schema authors. It covers the schema file format, all mapping options, all datatypes, the translation and validator systems, and the helper functions available inside translation functions.

## Table of Contents

- [Schema Files](#schema-files)
- [Mappings](#mappings)
  - [Mapping Options](#mapping-options)
  - [Fuzzy Variables](#fuzzy-variables)
  - [Aliases](#aliases)
  - [Collections](#collections)
- [Datatypes](#datatypes)
- [Translations](#translations)
- [Validators](#validators)
- [Helper Functions](#helper-functions)
- [The conf File Format](#the-conf-file-format)
- [The Pipeline](#the-pipeline)

---

## Schema Files

A schema file is a plain Erlang terms file (parsed with `erl_scan`/`erl_parse`). Each term is one of three types:

```erlang
{mapping, "conf.key", "app.erlang_key", [Options]}.
{translation, "app.erlang_key", fun(Conf) -> ... end}.
{validator, "name", "description", fun(Value) -> boolean() end}.
```

Multiple schema files can be loaded together. Later files can override earlier ones using the `merge` option. Schema files are loaded via `cuttlefish_schema:files/1` or `cuttlefish_schema:strings/1`.

---

## Mappings

A mapping connects a conf file key to an Erlang app.config key.

```erlang
{mapping, "ring_size", "riak_core.ring_creation_size", [
    {datatype, integer},
    {default, 64},
    {doc, ["The number of partitions in the ring."]}
]}.
```

The first argument is the conf key (dot-separated string). The second is the Erlang app.config path (also dot-separated; the first segment is the application name). The third is a proplist of options.

### Mapping Options

| Option | Value | Description |
|--------|-------|-------------|
| `{datatype, T}` | datatype or list of datatypes | Type(s) for parsing and validation. See [Datatypes](#datatypes). |
| `{default, V}` | any Erlang term | Value used when the key is absent from the conf file. |
| `{commented, V}` | any Erlang term | Like `default`, but the key is commented out in the generated conf file. |
| `{level, L}` | `basic`, `intermediate`, or `advanced` | Controls which keys appear in the generated conf file. Defaults to `basic`. |
| `{doc, [String]}` | list of strings | Documentation lines shown in the generated conf file. |
| `{validators, [Name]}` | list of strings | Named validators to run against the parsed value. |
| `{hidden, true}` | boolean | Omit this key from the generated conf file entirely. |
| `{alias, "old.key"}` | string | A single deprecated conf key that maps to this mapping. See [Aliases](#aliases). |
| `{aliases, ["old.key"]}` | list of strings | Multiple deprecated conf keys. See [Aliases](#aliases). |
| `{collect, Type}` | collect type | Auto-collect fuzzy variable matches into a list, map, or proplist. See [Collections](#collections). |
| `{see, ["other.key"]}` | list of strings | Cross-references to related conf keys, shown in generated conf. |
| `{include_default, "name"}` | string | Includes a named default value in the generated conf. |
| `merge` | atom | Merge this mapping with an existing mapping of the same variable rather than replacing it. Used when multiple schema files contribute to the same key. |

### Fuzzy Variables

A conf key segment prefixed with `$` is a fuzzy variable - it acts as a wildcard matching any value in that position.

```erlang
{mapping, "listener.tcp.$name", "ranch.tcp_listeners", [
    {datatype, integer}
]}.
```

This matches `listener.tcp.default`, `listener.tcp.internal`, etc. The `$name` segment captures the concrete value. Fuzzy variables are used with translations (to collect all matching values) or with the `{collect, Type}` option (to auto-collect without a translation).

Fuzzy variables are not supported in aliases.

### Aliases

Aliases allow a mapping to transparently accept deprecated conf keys. When a user's conf file contains an alias key, cuttlefish rewrites it to the canonical key and logs a deprecation warning.

```erlang
{mapping, "new.key", "app.setting", [
    {datatype, integer},
    {default, 42},
    {alias, "old.key"}
]}.
```

Multiple aliases:

```erlang
{mapping, "new.key", "app.setting", [
    {datatype, integer},
    {alias, "old.key"},
    {aliases, ["older.key", "oldest.key"]}
]}.
```

**Behavior:**

- Alias resolution runs before defaults and substitutions.
- If both the canonical key and an alias key are present in the conf file, the canonical key wins.
- If multiple alias keys are present and the canonical key is absent, the first alias in declaration order wins.
- Using `$(old.key)` in an RHS substitution is an error. Use `$(new.key)` instead.
- Aliases on fuzzy variables are not supported and are rejected at schema load time.
- In a merge mapping, `{aliases, []}` explicitly clears all inherited aliases.

**Validation at schema load time:**

- An alias key that shadows a canonical key is rejected.
- An alias key claimed by two different mappings is rejected.

### Collections

The `{collect, Type}` option eliminates the boilerplate translation function required for fuzzy-variable mappings that collect values into a list, map, or proplist.

**Supported collection types:**

| Type | Erlang result | Key conversion |
|------|---------------|----------------|
| `list` | `[Value]` | none - values sorted by wildcard segment |
| `{map, atom}` | `#{atom() => Value}` | `list_to_atom/1` |
| `{map, binary}` | `#{binary() => Value}` | `list_to_binary/1` |
| `{proplist, atom}` | `[{atom(), Value}]` | `list_to_atom/1`, sorted by wildcard segment |

**Example - replacing an explicit translation:**

Before:

```erlang
{mapping, "auth_mechanisms.$name", "rabbit.auth_mechanisms", [
    {datatype, atom}
]}.

{translation, "rabbit.auth_mechanisms",
fun(Conf) ->
    Settings = cuttlefish_variable:filter_by_prefix("auth_mechanisms", Conf),
    Sorted = lists:keysort(1, Settings),
    [V || {_, V} <- Sorted]
end}.
```

After:

```erlang
{mapping, "auth_mechanisms.$name", "rabbit.auth_mechanisms", [
    {datatype, atom},
    {collect, list}
]}.
```

**Sorting:** For `list` and `{proplist, atom}`, values are sorted lexicographically by the wildcard segment value (e.g. the `$name` part). For a single-segment fuzzy variable this produces the same order as `lists:keysort/1` on the full conf key.

**Empty result:** When no conf values match the wildcard pattern, the key is absent from the generated app.config (equivalent to calling `cuttlefish:unset()` in a translation). This differs from a translation that returns `[]`, which writes an empty list to app.config.

**Translation takes precedence:** If a translation exists for the same Erlang app.config key, the translation always runs and `{collect, Type}` is ignored. This means existing schemas with translations are unaffected when `{collect, Type}` is added to a mapping.

**Constraints:**

- `{collect, Type}` requires exactly one fuzzy (`$`) segment in the variable name. Zero or more than one wildcard is a schema load-time error.
- `{proplist, binary}` is not a supported collection type.

---

## Datatypes

The `{datatype, T}` option specifies how a conf string value is parsed into an Erlang term. Multiple datatypes can be specified as a list; cuttlefish tries each in order and uses the first that succeeds.

### `integer`

Parses a decimal integer string. Erlang type: `integer()`.

```
ring_size = 64
```

### `string`

Passes the value through as-is. Erlang type: `string()` (charlist).

```
node.name = riak@127.0.0.1
```

### `binary`

Converts the string to a binary. Erlang type: `binary()`.

```
secret.key = mysecret
```

### `atom`

Converts the string to an atom via `list_to_atom/1`. Erlang type: `atom()`.

```
storage_backend = bitcask
```

### `float`

Parses a floating-point string. Erlang type: `float()`.

```
threshold = 0.75
```

### `flag`

Parses `on`/`off` to `true`/`false`. Erlang type: `boolean()`.

```
log.syslog = on
```

Custom on/off atoms:

```erlang
{datatype, {flag, enabled, disabled}}
```

Parses `enabled`/`disabled` to `true`/`false`.

Custom on/off values:

```erlang
{datatype, {flag, {on, tyk}, {off, torp}}}
```

Parses `on`/`off` to `tyk`/`torp`.

### `{enum, [atom()]}`

Parses one of a fixed set of atom values.

```erlang
{datatype, {enum, [debug, info, warning, error]}}
```

```
log.level = info
```

Erlang type: `atom()`.

### `ip`

Parses an `IP:Port` string. Erlang type: `{string(), integer()}`.

```
listener.http.internal = 127.0.0.1:8098
```

### `fqdn`

Parses a hostname with optional port. Erlang type: `string()` or `{string(), integer()}`.

```
amqp.hostname = rabbit.example.com:5672
```

### `domain_socket`

Parses a Unix domain socket path. Accepts either a plain path string or `local:PATH:PORT` format. Erlang type: `string()` or `{local, string(), integer()}`.

```
listener.unix = /var/run/app.sock
listener.unix = local:/var/run/app.sock:0
```

### `bytesize`

Parses a byte size with optional unit suffix (`KB`, `MB`, `GB`). Case-insensitive. Erlang type: `integer()` (bytes).

```
object.size.maximum = 50MB
```

### `{duration, Unit}`

Parses a duration string (e.g. `5s`, `2h30m`). Erlang type: `integer()` (in the specified unit).

Units: `ms` (milliseconds), `s` (seconds), `m` (minutes), `h` (hours), `d` (days), `w` (weeks), `f` (fortnights).

```erlang
{datatype, {duration, ms}}
```

```
handoff.timeout = 30s
```

### `{percent, integer}` and `{percent, float}`

Parses a percentage string ending in `%`. `{percent, integer}` produces an `integer()` in the range 0-100. `{percent, float}` produces a `float()` in the range 0.0-1.0.

```
cache.fill.ratio = 75%
```

### `tagged_string`

Parses a `tag:value` string where the tag is alphanumeric (plus underscores). Erlang type: `{atom(), string()}`.

```
credential = plain:mypassword
```

### `tagged_binary`

Like `tagged_string` but the value part is returned as a binary. Erlang type: `{atom(), binary()}`.

### `{list, T}`

Parses a comma-separated list of values, each parsed as datatype `T`. Erlang type: `[T]`. Lists of lists are not supported.

```erlang
{datatype, {list, atom}}
```

```
plugins = plugin_a, plugin_b, plugin_c
```

### `file` and `directory`

Pass the value through as a string. Semantically indicate that the value is a filesystem path. Erlang type: `string()`.

---

## Translations

A translation converts one or more conf values into a final Erlang app.config value. Translations run after datatype conversion, so values in `Conf` are already typed Erlang terms.

```erlang
{translation, "app.erlang_key",
fun(Conf) ->
    Value = cuttlefish:conf_get("some.conf.key", Conf),
    transform(Value)
end}.
```

The translation function receives the full typed conf proplist. It must return the Erlang value to write to app.config.

To omit the key from app.config entirely, call `cuttlefish:unset/0`. To report invalid input, call `cuttlefish:invalid/1,2`. Both are throws caught by the pipeline.

When a translation exists for an Erlang key, it takes precedence over any direct 1:1 mapping to that key.

---

## Validators

A validator is a named predicate applied to a parsed value before translations run.

```erlang
{validator, "positive_integer", "must be a positive integer",
fun(Value) ->
    Value > 0
end}.
```

Reference validators by name in a mapping's `validators` list:

```erlang
{mapping, "ring_size", "riak_core.ring_creation_size", [
    {datatype, integer},
    {validators, ["positive_integer"]}
]}.
```

The validator function receives the typed value (after datatype conversion). Return `true` to pass, `false` to fail. On failure, cuttlefish reports the validator description as the error message.

---

## Helper Functions

These functions are available inside translation functions via the `cuttlefish` module.

### `cuttlefish:conf_get/2`

```erlang
cuttlefish:conf_get(Key, Conf) -> Value
```

Looks up `Key` in `Conf`. `Key` can be a dot-separated string or a tokenized variable list. Throws `{not_found, Key}` if the key is absent, which aborts the translation. Use `conf_get/3` if you want a default instead.

### `cuttlefish:conf_get/3`

```erlang
cuttlefish:conf_get(Key, Conf, Default) -> Value
```

Like `conf_get/2` but returns `Default` instead of throwing when the key is absent.

### `cuttlefish:unset/0`

```erlang
cuttlefish:unset() -> no_return()
```

Call inside a translation to omit the Erlang key from the generated app.config entirely.

### `cuttlefish:invalid/1,2`

```erlang
cuttlefish:invalid(Reason :: string()) -> no_return()
cuttlefish:invalid(Fmt :: io:format(), Args :: [term()]) -> no_return()
```

Call inside a translation to report that the configuration is invalid. `Reason` is shown to the user.

### `cuttlefish:warn/1,2`

```erlang
cuttlefish:warn(Message :: iodata()) -> ok
cuttlefish:warn(Fmt :: io:format(), Args :: [term()]) -> ok
```

Call inside a translation to log a warning without aborting.

### `cuttlefish:otp/2,3`

```erlang
cuttlefish:otp(MinVersion :: string(), IfGte :: any(), IfLt :: any()) -> any()
cuttlefish:otp(MinVersion :: string(), ActualVersion :: string()) -> boolean()
```

OTP version comparison helper. Useful for generating version-dependent configuration.

```erlang
{translation, "app.ssl_opts",
fun(Conf) ->
    Base = [...],
    cuttlefish:otp("26", [{versions, ['tlsv1.3', 'tlsv1.2']} | Base], Base)
end}.
```

---

## The conf File Format

Conf files use a sysctl-style `key = value` syntax.

```ini
ring_size = 64
log.error.file = /var/log/error.log
log.syslog = on
```

### Comments

Lines beginning with `#` are comments.

```ini
## This is a comment
# This is also a comment
ring_size = 64
```

### Multiline Values

Values spanning multiple lines use triple single-quote delimiters (`'''`).

```ini
api.config = '''
{
  "endpoints": ["/health", "/metrics"],
  "timeout": 30
}
'''
```

Leading and trailing newlines inside the delimiters are trimmed. Internal whitespace and newlines are preserved exactly.

### Includes

One conf file can include another:

```ini
include /etc/app/extra.conf
```

Glob patterns are supported:

```ini
include conf.d/*.conf
```

### RHS Substitutions

A value can reference another conf key using `$(key)` syntax:

```ini
listener.http.internal = 127.0.0.1:8098
listener.http.external = $(listener.http.internal)
```

Substitutions are resolved after alias rewriting. Using an alias key in a substitution (e.g. `$(old.key)`) is an error; use the canonical key instead.

### Unicode BOM

Conf files starting with a UTF-8, UTF-16 BE/LE, or UTF-32 BE/LE byte order mark are handled correctly. The BOM is stripped before parsing.

---

## The Pipeline

Understanding the pipeline helps when debugging unexpected behavior.

1. **Parse conf files** - `cuttlefish_conf:file/1` parses `.conf` into a `[{variable(), string()}]` proplist. Values are raw strings at this stage.

2. **Load schema files** - `cuttlefish_schema:files/1` parses `.schema` files, merges them, validates aliases, and returns a `{[translation()], [mapping()], [validator()]}` tuple.

3. **Resolve aliases** - Alias keys in conf are rewritten to their canonical keys. Deprecation warnings are logged. Canonical key wins if both are present.

4. **Add defaults** - Schema defaults are injected for any key absent from conf.

5. **Expand substitutions** - `$(key)` references in values are resolved.

6. **Convert datatypes** - String values are converted to typed Erlang terms using each mapping's declared datatype.

7. **Run validators** - Named validators are applied to typed values.

8. **Run translations** - Translation functions produce the final Erlang values. Direct 1:1 mappings (and `{collect, Type}` mappings) are applied here as well.

The output is a `[{AppName, [{Key, Value}]}]` proplist suitable for use as `app.config`.
