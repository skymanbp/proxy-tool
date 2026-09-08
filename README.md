# proxy-tool

Run a CLI coding agent — Claude Code, Codex, Kimi Code — against whatever model
provider you like, chosen from a terminal UI.

Two panes pick an agent and a provider. A form edits the provider's **base
URL, model, reasoning effort and credential**. Pressing Enter hands the
terminal to the agent, wired to that endpoint; when the agent exits you land
back in the menu.

Where the agent and the provider speak the same API dialect, the agent is
pointed straight at it. Where they don't, a loopback proxy translates in
between — which is what lets Claude Code drive an OpenAI-compatible endpoint.

```
┌─Agents───────────────────┬─Providers────────────────────────┐
│ > Claude Code (anthropic)│ > Moonshot / Kimi   (anthropic)  │
│   Codex CLI (openai-chat)│   OpenAI (official) (openai-chat)│
│   Kimi Code   (anthropic)│   Local server      (openai-chat)│
├──────────────────────────┴──────────────────────────────────┤
│ Launch preview                                              │
│ agent    : Claude Code  (claude, speaks Anthropic Messages) │
│ route    : auto -> local proxy, translating ...             │
│ endpoint : http://127.0.0.1:<auto>                          │
│ model    : kimi-k2-turbo-preview                            │
│ effort   : high                                             │
│ key      : env:MOONSHOT_API_KEY                             │
└─────────────────────────────────────────────────────────────┘
 Tab:pane Enter:launch n:new e:edit d:delete r:route b:binding L:language s:save
```

## Build

Needs [Stack](https://docs.haskellstack.org/). Everything else is pinned by
`stack.yaml` (lts-24.46 / GHC 9.10.3).

```sh
stack build          # library, executable and tests
stack test           # 122 examples
stack exec proxy-tool
```

## Use

```
proxy-tool                                  open the terminal UI
proxy-tool list                             list configured agents and providers
proxy-tool plan <agent> <provider>          show what a launch would do, and stop
proxy-tool run  <agent> <provider>          launch the agent
proxy-tool doctor                           check config, commands and credentials
proxy-tool config path|show|init

  --lang en|zh                              interface language for this run
  --route auto|direct|proxy                 override the route for one launch
```

`plan` and `doctor` are the ones to reach for when something looks wrong: they
print the exact command line, the exact environment and the route decision
without starting anything. Credentials are masked in both.

Config lives in the platform config directory (`proxy-tool/config.json`);
`PROXY_TOOL_CONFIG` overrides the path.

## Language

The interface is **English by default and switchable to Chinese** — `L` in the
TUI, `--lang zh` on the command line, or `"lang": "zh"` in the config.

| source | wins over | persisted? |
|---|---|---|
| `--lang en\|zh` | everything | yes — it is an explicit choice |
| `L` in the TUI | the config file | yes |
| `PROXY_TOOL_LANG` | the config file | **no** — scoped to one run |
| `"lang"` in `config.json` | the default | it *is* the stored value |
| nothing | — | English |

A config written before the field existed still loads, in English.

Messages are a typed catalogue rather than lookup keys: the library returns
`Msg` values and only the edge renders them, so a message carries its
parameters instead of being pre-formatted somewhere that does not yet know the
language. Both renderers are total over `Msg` with no catch-all, which makes a
missing translation a **compile error** rather than a blank line or an English
sentence in a Chinese screen.

Deliberately *not* translated: protocol names, model names, effort levels and
route names — they are identifiers you type back in — and the JSON error
bodies the proxy returns to an agent, which are read by a program.

Labels are padded by display width, not character count, so columns line up in
a language whose characters occupy two cells. That width function is checked
against the one vty lays out with, over every string the interface renders.

## Routing

| route | behaviour |
|---|---|
| `direct` | agent talks straight to the provider; refused if the dialects differ |
| `proxy`  | always go through the loopback proxy |
| `auto`   | proxy only when the dialects differ (default) |

Forcing `proxy` is useful even when the dialects match: only the proxy can
override the model on every request and inject a reasoning effort the agent
has no flag for.

**Translation is one-directional.** Anthropic-speaking agent → OpenAI-compatible
provider is implemented. The reverse (an OpenAI-speaking agent such as Codex
against an Anthropic-native endpoint) is **not**, and is refused at plan time
with an explanation rather than failing once the agent is already running.
Codex against any OpenAI-compatible provider works directly.

## What the proxy does

- **Forces the model.** Agents hardcode model names; the proxy rewrites them.
- **Injects effort.** On the OpenAI wire as `reasoning_effort`; on the
  Anthropic wire as an extended-thinking budget, raising `max_tokens` so the
  budget stays below it and dropping the sampling parameters the API refuses
  alongside thinking.
- **Substitutes a cheap tier.** A provider may name a `fast_model`, used when
  the agent asks for a haiku/mini/flash-class model.
- **Translates**, including streaming: an OpenAI chat stream becomes a
  well-formed Anthropic event stream (`message_start`, text and `tool_use`
  content blocks, `input_json_delta`, `message_delta` with real token counts,
  `message_stop`). Tool-call ids pass through unchanged so results round-trip.
- **Escape hatch.** A provider's `extra_body` is merged into every request and
  wins over everything else, for knobs this tool does not model.

### Security properties

- The proxy binds **loopback only** and requires a per-session bearer token,
  generated per launch, so another local process cannot use it as a free relay
  to your paid API. The provider key never reaches the agent — it stays in the
  proxy; the agent gets the session token.
- **The binding owns its variables.** Every environment variable an agent's
  binding names is removed from the inherited environment before the computed
  values are applied. Without that, an `ANTHROPIC_API_KEY` left over in your
  shell would be handed to whichever third party you just selected.
- Redirects are not followed, so an injected credential cannot be bounced to a
  host you never configured.
- `config.json` is written owner-read/write. On POSIX that is a real `0600`;
  on Windows the `directory` package can only express the read-only attribute
  and the inherited NTFS ACL is what actually restricts access — so prefer
  `env:NAME` over pasting a key.

## Agents are data, not code

An agent is a record saying which executable to run and which environment
variables and arguments carry the endpoint, credential, model and effort.
Templates (`{base_url}`, `{key}`, `{model}`, `{fast_model}`, `{effort}`,
`{effort_openai}`, `{thinking_budget}`, `{provider}`) expand at launch. All of
it is editable in the TUI under `b`.

This matters because these CLIs change their flags between releases. When that
happens the fix is an edit, not a rebuild.

An argument whose placeholder resolves to empty is dropped along with the bare
flag in front of it — so leaving effort unset simply omits
`-c model_reasoning_effort` rather than sending an empty one.

### Provenance of the shipped bindings

Because guessing here produces a tool that silently does nothing:

- **Claude Code** — `ANTHROPIC_BASE_URL`, `ANTHROPIC_AUTH_TOKEN`,
  `ANTHROPIC_API_KEY`, `ANTHROPIC_MODEL`, `ANTHROPIC_SMALL_FAST_MODEL` and
  `ANTHROPIC_DEFAULT_HAIKU_MODEL` were read out of the installed `claude`
  binary.
- **Codex** — `-c` / `-m` from `codex --help` (v0.153.3); the config keys
  `model_provider`, `model_providers`, `base_url`, `env_key`, `wire_api` and
  `model_reasoning_effort` read out of the installed `codex` binary. Values are
  deliberately unquoted: codex parses a `-c` value as TOML and falls back to a
  raw string, which keeps arguments intact through a Windows `.cmd` shim.
- **Kimi Code** — **unverified.** No Kimi CLI was installed on the machine this
  was developed on, so its binding is a starting point, flagged as such in the
  launch preview. Running Claude Code against the Moonshot provider is the path
  that was actually exercised.
- Wire formats were checked against the installed `claude` and `codex` binaries
  and against openai-python 2.30.0, not from memory.

## Bugs found and fixed

Real defects hit while building and debugging this, recorded so they are not
rediscovered. Each was confirmed on the wire or in a library's source, not
inferred.

**HTTP layer**

- **`Accept-Encoding: gzip` sent behind our back.** http-client inserts it
  when the header is absent (`Network/HTTP/Client/Request.hs`). With
  decompression disabled and `content-encoding` stripped from the response, a
  provider that honoured it would have handed the agent a gzip stream labelled
  as plain text. The proxy now sends `accept-encoding: identity` explicitly.
- **Duplicate `Content-Length`.** http-client prepends its own; the proxy was
  also copying the client's, putting the field on the wire twice — which
  RFC 9110 §8.6 lets a recipient reject outright. The manual header is gone.
- **Unbounded SSE reassembly.** An upstream that never sent a newline would
  grow the partial-line buffer without limit. There is now a cap.
- **A revisited tool-call index** silently produced two `tool_use` blocks for
  one call. It is now tracked and surfaced as a warning instead.
- **Prompt tokens were collected and then dropped.** OpenAI reports usage only
  in a trailing chunk, by which time `message_start` has already gone out
  claiming zero input tokens. The count was absorbed into the stream state and
  never re-emitted, so an agent that displays or bills on token usage saw
  `input_tokens: 0` for every request — the larger half of the cost.
  `message_delta` now carries both counts, which is what its `usage` object is
  documented to hold.

**Launch**

- **`{effort}` was not substituted** in an agent's own `args`, only in its
  binding's, so the placeholder reached the command line literally.

**Windows**

- **`.cmd` shims.** `System.Directory.findExecutable` only ever appends
  `.exe`, so the `.cmd` wrappers npm installs — how `codex` arrives on Windows
  — are invisible to it (`findExecutable "codex"` returns `Nothing` while
  `findExecutable "codex.cmd"` succeeds). `resolveCommand` walks `PATHEXT`
  itself and runs a batch file through `cmd.exe /c`, since `CreateProcess`
  cannot execute one directly.
- **Build output encoding.** GHC writes diagnostics containing `•` and smart
  quotes; Stack re-emits a dependency's compiler output on a stderr handle
  encoded with the machine's ANSI code page, and on a code page that cannot
  represent them (936 here; 1252 has the same hole) the build aborts with
  `commitAndReleaseBuffer: invalid argument (cannot encode character
  '\8226')`. `stack.yaml` therefore silences warnings for snapshot packages and
  restores the full set for this package. If you still hit it on an *error*,
  read it with `stack ghc -- -fno-code -isrc <file>`, which lets GHC write to
  your terminal directly.
- **A Chinese interface threw instead of printing.** GHC encodes the standard
  handles with the machine's code page, so `--lang zh` on a machine whose code
  page cannot represent the characters died with
  `commitAndReleaseBuffer: invalid argument (cannot encode character
  '\35753')` and produced no output at all — the same failure mode as the
  build-output one above, in our own program. The handles are now UTF-8, and
  on Windows the console is put into the UTF-8 code page for the lifetime of
  the process and **put back afterwards**, since the code page outlives the
  process and belongs to the user's shell.

## Tests

`stack test` — 122 examples covering request and response translation, the
streaming state machine (block boundaries, indices, tool-call assembly, the
usage-only chunk with both token counts, truncated upstreams), SSE framing
across chunk boundaries, config round-tripping, routing decisions, argument
dropping, environment ownership, and the message catalogue (language parsing,
display widths, and — for every message that takes parameters — that both
languages keep them).

Routing and warning tests assert on the *message*, not on its English wording,
so rewording the English cannot break them and a wrong Chinese string cannot
hide behind a passing English one.

Beyond the unit tests, the whole path was exercised end to end against a stub
provider, once per language plus once with a config that predates the language
field: translated request upstream, translated stream back, real token counts
in `message_delta`, `401` on a bad session token, a planted
`ANTHROPIC_API_KEY` confirmed not to leak, a single `content-length` on the
wire, `accept-encoding: identity` reaching upstream, byte-identical
passthrough in same-dialect mode, and an agent launched through a `.cmd` shim.

## Layout

| module | role |
|---|---|
| `ProxyTool.Types` | vocabulary: wires, effort, secrets, providers, agent bindings |
| `ProxyTool.I18n` | the message catalogue, both languages, and display widths |
| `ProxyTool.Config` | on-disk config, shipped defaults, lookups and edits |
| `ProxyTool.Translate` | pure rewriting and translation, including the SSE state machine |
| `ProxyTool.Proxy` | the loopback HTTP server |
| `ProxyTool.Launch` | planning, command resolution, process spawning |
| `ProxyTool.Session` | brings the proxy up, runs the agent, tears down |
| `ProxyTool.TUI` | the interface |

Planning is pure and total, so the TUI can render exactly what is about to
happen before anything is spawned, and the same computation is unit-tested.
