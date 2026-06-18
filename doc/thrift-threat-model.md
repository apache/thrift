# Apache Thrift — Threat Model (v2 draft)

## §1 Header

- **Project:** Apache Thrift (`apache/thrift`)
- **Scope:** the `apache/thrift` repository only. The `apache/thrift-website` repository is **excluded at PMC's go-ahead** *(maintainer — Jens 2026-05-19; the Security team recommended dropping the website on triage-cost-vs-value grounds, PMC concurred).*
- **Version / commit binding:** authored against `apache/thrift`'s `master` branch at the time of drafting (2026-05). A report against project version *N* is triaged against this model as it stood at *N*, not at `HEAD`. Re-bind on each release.
- **Authors:** drafted by the ASF Security Team (Glasswing pre-scan threat-model pass). Wave-1 maintainer review completed by Jens Geyer 2026-05-19/20; promotion of remaining *(inferred)* tags pending PMC-wide review.
- **Status:** **DRAFT v2 — Wave 1 partially ratified by Jens Geyer; pending Thrift PMC-wide review.** 
- **Reporting cross-reference:** at the time of drafting Thrift's repo has **no `SECURITY.md`**, and the project website exposes only the generic `apache.org/security` pointer rather than a project-specific page. Until a per-project disclosure document is added, findings should be reported via the ASF security PMC pipeline (`security@apache.org`) per ASF policy. Findings against §8 (claimed properties) are in-scope vulnerabilities; findings that fall under §3 or §9 will be closed citing this document.
- **Provenance legend:**
  - *(documented)* — stated in the project's own artifacts: in-repo `README*`, `CHANGES.md`, `FUZZING.md`, `LANGUAGES.md`, `AGENTS.md`, `doc/specs/*`, per-binding `lib/<lang>/README*`, header comments, or JIRA tickets cited in `CHANGES.md`.
  - *(inferred)* — reasoned from code shape, the existence/absence of a defense, or general domain knowledge for RPC framing/codec/runtime libraries. Every *(inferred)* claim has a numbered match in §14.
  - *(maintainer)* — explicitly **not used** in this draft. The Thrift PMC has not yet ratified anything here.
- **Draft confidence (claim count):** **78 documented / 0 maintainer / 81 inferred** across §1–§13; §14 carries **77 numbered open questions** and every *(inferred)* tag is back-referenced to one of them by number. This is a *draft-first* model in the `SKILL.md` §3.2 sense — published artifacts (the specs, FUZZING, CHANGES, per-binding READMEs) carry a high *(documented)* count, but every assertion about *intent*, *adversary model*, *resource-bound policy*, *cross-binding parity*, and *what counts as a bug* is *(inferred)* until the PMC confirms.
- **What Thrift is (one paragraph):** Apache Thrift is an IDL-driven cross-language RPC and serialization framework. A user writes service and struct definitions in the Thrift IDL (`.thrift` files); the **C++ `thrift` compiler** (under `compiler/cpp/`) emits client and server **generated code** for any of ~28 target languages. Each language's `lib/<lang>/` runtime supplies the **protocol** layer (Binary, Compact, JSON, Multiplex), the **endpoint transport** layer (Socket, TLS/SSL, Domain socket, Pipe, Memory, File), the **layered transport** wrappers (Framed/Buffered/Header/HTTP/zlib), and the **server** loop (Simple, Threaded, ThreadPool, Nonblocking, Forking). Downstream applications link the runtime + generated code; Thrift itself is **a library + code generator, not a deployed service**.

---

## §2 Scope and intended use

Apache Thrift is an **IDL-driven, polyglot RPC + serialization toolkit**. The primary use cases *(documented — `LANGUAGES.md`, per-binding READMEs)*:

1. Author a service in IDL (`.thrift` file), generate client + server stubs in language A and language B, and let them call each other across a process boundary.
2. Use the runtime's `TProtocol`/`TTransport` stack as a **standalone serializer/deserializer** for IDL-defined struct types — i.e., encode-to-bytes / decode-from-bytes without RPC.
3. Embed the generated client or server into a host application that handles its own deployment, authentication policy, TLS posture, listening surface, and request lifecycle.

Thrift is **not a deployed service.** There is no `thriftd`. The `compiler/cpp/thrift` binary is invoked at build time, not at run time. Threats land in code that downstream applications choose to embed — the runtime (`lib/<lang>/`) and the generated stubs (`compiler/cpp/src/thrift/generate/t_<lang>_generator.cc` output).

### Caller / actor roles

Because Thrift ships server *machinery* but no opinionated server, a downstream deployment has several actors. These do not exist *in* Thrift, but every threat statement in §7 attaches to one of them.

- **Application developer** — writes `.thrift` files, runs the compiler, links the runtime, writes the service handler. Trusted at the compile-time boundary (they pick the IDL, the bindings, the build flags).
- **Operator** — runs the resulting binary. Configures TLS material, network exposure, server type, and any limits the runtime exposes (`maxMessageSize`, `maxFrameSize`, container limits). Trusted for that instance.
- **Client peer** — the entity sending bytes to a Thrift server's wire. **Untrusted by default** *(maintainer — Q1)*: bytes on the socket are attacker-controllable unless the operator has imposed an authentication layer.
- **Server peer** — the entity returning bytes to a Thrift client. Symmetric: a Thrift client deserializes responses from a server it does not control in the general case, so server-to-client replies are also attacker-controllable *(maintainer — Q2)*.
- **In-process caller of the serializer** — code that hands bytes to `TBinaryProtocol::readStruct()` (or its per-language equivalent) without a network in between (e.g., reading a file, decoding a queue message). Same trust posture as a client peer: the bytes are untrusted, the calling code is trusted.

### Component-family table

Thrift is not a monolith; each row below has a measurably different threat profile and is modeled separately in §6/§8.

| Family | Representative entry point | Touches outside the process? | In model? |
| --- | --- | --- | --- |
| **IDL compiler** (`compiler/cpp/`) | `thrift -gen <lang> file.thrift` | Reads `.thrift` files, writes generated source. No network, no sockets at run-time. | **In model**, but threat profile is a build-time text-processor (see §7). |
| **C++ runtime** (`lib/cpp/`, `libthrift`, `libthriftnb`) | `TBinaryProtocol`, `TCompactProtocol`, `TJSONProtocol`, `TSocket`, `TSSLSocket`, `T*Server` | Sockets, files, threads, pipes, named pipes (Windows), TLS via OpenSSL, optional libevent. | **In model.** Memory-unsafe core; highest-blast-radius family. |
| **c_glib runtime** (`lib/c_glib/`) | C bindings via GLib | Same as C++. | **In model.** Same memory-safety class as C++. |
| **JVM runtime** (`lib/java/`) | `TBinaryProtocol`, `TFramedTransport`, `TThreadedSelectorServer`, SASL via `javax.security.sasl` | Sockets, TLS via JSSE, SASL. | **In model.** Memory-safe runtime; framing bugs still cause OOMs / DoS. |
| **Python runtime** (`lib/py/`) | `thrift.protocol.TBinaryProtocol`, `thrift.transport.TSocket`, `thrift.server.*` | Sockets, TLS via `ssl` stdlib, optional Tornado / asyncio variants. | **In model.** Memory-safe; deserializer DoS bugs still relevant. |
| **Go runtime** (`lib/go/`) | `thrift.NewTBinaryProtocol`, `thrift.NewTSocket`, `thrift.NewTSimpleServer` | Sockets, TLS via `crypto/tls`. | **In model.** Memory-safe; past stack-overflow on recursive skip *(documented — THRIFT-5469)*. |
| **Rust runtime** (`lib/rs/`) | `thrift::protocol::*`, `thrift::transport::*`, `thrift::server::*` | Sockets, TLS where wired up. | **In model.** Memory-safe; past capacity-overflow on server side *(documented — THRIFT-5812)*. |
| **Other language runtimes** (`lib/{netstd,cl,d,dart,delphi,erl,haxe,javame,js,kotlin,lua,nodejs,nodets,ocaml,perl,php,rb,st,ts}/`) | Per-binding `T*Protocol` / `T*Transport` / `T*Server` | Each: language-stdlib sockets + TLS. | **In model.** Per-binding threat surface; assumed analogous to the canonical four (C++/Java/Python/Go) but not identical. See §14 Q26-Q30. *(maintainer — Jens 2026-05-20: list re-swept against `apache/thrift/lib/` HEAD; `cl`, `delphi`, `haxe`, `nodets`, `ts` added; `hs` removed; `dlang` renamed to `d`; `swift-pkg` consolidated into `swift`; `ts-js` renamed to `ts`. Jens 2026-06-18: `swift` removed — binding dropped per THRIFT-5864.)* |
| **Generated code** (output of `thrift -gen <lang>`) | `XService.Iface`, `XService.Client`, `XService.Processor`, struct `read()`/`write()` | Whatever the generator emits — typically pure (no I/O) deserializer + dispatcher glue. | **In model**, but threat profile is "whatever the generator templates produce"; bugs in *generated* code that vanish on regeneration are still in-model. |
| **Wire protocols (specs)** (`doc/specs/thrift-binary-protocol.md`, `…-compact-protocol.md`, `…-json-protocol.md`, `…-rpc.md`) | N/A — these define the encoding. | N/A. | **In model** as the contract that all runtimes must implement; ambiguity in the spec is a model-level concern. |
| **Test, tutorial, contrib** (`test/`, `tutorial/`, `contrib/`) | Example clients/servers, cross-language test driver, third-party utilities. | Yes — they open sockets, read files. | **Out of model** *(maintainer — Q3)*. See §3. |
| **Generated bindings in third-party language ports** (e.g., `vcpkg` port, `crates.io` packaging, Maven Central artifact) | N/A — packaging. | N/A. | **Out of model** for the *packaging mechanism* (vcpkg is "not the Apache Thrift project's responsibility" *(documented — `lib/cpp/README.md`)*); the code inside is in-model. |

The four "canonical" runtimes (C++, Java, Python, Go) plus Rust account for the bulk of `CHANGES.md` security-tagged entries; the rest of the bindings inherit the model by *analogy* until §14 questions resolve per-binding parity. **Caveat *(maintainer — Jens 2026-05-20):*** the "canonical bindings define the contract" framing is the *default* and not a strict invariant — features can and do originate in non-canonical bindings. The `uuid` type, for example, was first implemented in C# `netstd` and then propagated outward. Where a non-canonical binding is the contract origin, that fact is recorded inline at the relevant property (§8 / §9) rather than implied away by analogy.

---

## §3 Out of scope (explicit non-goals)

These are the use cases and threat classes Thrift does **not** aim to handle. A finding that requires the model to defend against any of these closes as `OUT-OF-MODEL` per §13.

### Use cases out of scope

1. **A drop-in secure-by-default RPC endpoint.** Thrift does not ship a turnkey server with sane defaults that you can safely expose to the internet. The runtime gives an operator the components (`T*Server`, `TSSLSocket`, SASL transports, authentication-via-`AccessManager` hooks) and expects them to assemble a posture. *(maintainer — Q4)*
2. **A general-purpose schema-evolution format à la Avro/Protobuf-with-options.** Thrift's `required` vs `optional` semantics, field-id-only matching, and "unknown fields are silently skipped" behavior are wire-compatibility primitives, not a schema-registry contract *(documented — `doc/specs/thrift-binary-protocol.md` §Struct: "the fields can be encoded in any order [...] possible to handle unknown fields while decoding; these are simply ignored")*. Threats premised on "schema A and schema B must produce a verifiable mismatch" are out of model.
3. **A sandbox or process-isolation boundary.** A Thrift server hosts the application's handler code in-process; the handler sees deserialized objects directly. There is no isolation between the handler and the deserializer. *(maintainer — Q5)*
4. **A cryptographic primitive.** Thrift's TLS transports wrap OpenSSL (C++), JSSE (Java), `crypto/tls` (Go), the `ssl` stdlib (Python), etc.; the cryptographic guarantees are those of the underlying library, not Thrift. *(maintainer — Q6)*
5. **An input-validation framework for application-layer fields.** Thrift validates wire encoding (type tags, declared sizes); it does not validate that a `string name` is a valid email, that an `i32 limit` is non-negative, that a `list<UserId>` contains existing users, etc. That is the handler's job. *(maintainer — Q7)*
6. **A complete replacement for an authenticated transport.** A bare `TSocket` carries no authentication; SASL and TLS-client-cert are *available* (SASL in Java; client-cert via the C++ `AccessManager`) but not mandatory. *(documented — `lib/cpp/README.md` §Thrift/SSL)*

### Threats explicitly out of scope

7. **Attackers with control of the calling process.** An adversary who can call `TBinaryProtocol::readStruct` directly with a chosen pointer, or who can patch the generated processor's dispatch table, has already won. (Standard library-trust-boundary disclaimer.) *(maintainer — Q8)*
8. **Attackers with control of the IDL source fed to the compiler.** Generating code from an adversary-supplied `.thrift` file is the same threat class as feeding `bison`/`flex` adversary input: the compiler is a developer tool, not a sandboxed parser. *(maintainer — Q9)*
9. **Side-channel attacks against application data.** Timing leaks in `==` comparisons of secrets carried in Thrift structs, padding-oracle attacks against application-chosen ciphers, etc. Thrift provides no constant-time primitives. *(maintainer — Q10)*
10. **Supply-chain compromise of the build toolchain.** Compromise of the C++ compiler used to build `thrift`, of Gradle/Maven, of `go.mod` resolution, of the OpenSSL distribution, of `vcpkg` (out-of-scope packaging explicitly disclaimed *(documented)*). All belong to the host distro/build environment.
11. **DoS by raw socket-level pressure.** Slowloris, SYN flood, half-open-connection exhaustion, kernel-table exhaustion. These are operator/load-balancer concerns, not codec concerns. *(maintainer — Q11)*

### Code shipped in the repo but out of scope

The Thrift repo ships several directories of non-runtime code. The threat-model treatment:

- `test/` — cross-language test driver and per-language test clients/servers. Test code. **Out of model.** A finding in `test/cpp/src/TestServer.cpp` is `OUT-OF-MODEL: unsupported-component`.
- `tutorial/` — example clients and servers used as documentation/onboarding. **Out of model**, same disposition.
- `contrib/` — third-party utilities and adapters. **Out of model** unless a specific subdirectory is later promoted into a `lib/<lang>/` runtime. *(maintainer — Q3, Q12)*
- `compiler/cpp/test/` — compiler regression tests. **Out of model.**
- Hard-coded test material (e.g., `test/keys/server.crt`, `test/keys/server.key`, `test/keys/CA.pem` — *(documented — `lib/cpp/README.md` §How to run test client/server in SSL mode)*) — **Out of model.** These keys are demonstrably published; a finding that "the test key is private" is not a finding.
- Generated cross-language test code. **Out of model.**
- `apache/thrift-website` repo. **Out of model per PMC scope** for this engagement.

The boundary is **`lib/<lang>/`** (runtime), **`compiler/cpp/src/`** (compiler & code generators), and the **specs** (`doc/specs/`). Everything else is auxiliary.

---

## §4 Trust boundaries and data flow

Thrift presents **two trust boundaries**, neither of them well-marked in code:

### Boundary 1 — the wire (server side)

The bytes arriving at a `T*Server`'s socket are **untrusted**. Inside the runtime, those bytes flow through:

```
socket bytes
  └─ TTransport (TSocket / TSSLSocket / TFramedTransport / THttpServer / TBufferedTransport / TZlibTransport / THeaderTransport)
       └─ TProtocol (TBinaryProtocol / TCompactProtocol / TJSONProtocol / TMultiplexedProtocol)
            └─ generated Service.Processor (parses message header → struct args)
                 └─ generated struct.read(iprot)  ← unbounded deserialization without a per-call envelope size unless framed
                      └─ application handler  ← trust boundary back to the application
```

The trust transition is **between the protocol and the generated struct reader, and again between the struct reader and the application handler**. A finding in `TBinaryProtocol::readListBegin()` is in-scope; a finding in the application handler is the application's problem. *(maintainer — Q13 — confirm where the model places the boundary; we propose "the bytes are untrusted from the socket through the generated `read()` calls and become trusted only after the handler chooses to act on them".)*

### Boundary 2 — the wire (client side)

A Thrift **client** deserializes a response from a peer it does not necessarily trust. The flow is symmetric: untrusted bytes → transport → protocol → generated struct → application. The same memory-safety / DoS properties claimed for server-side reads must hold for client-side reads. Few clients defend against this in practice; the model should explicitly cover both directions. *(maintainer — Q14.)*

### Boundary 3 — the file/serializer (in-process)

Code that uses `TBinaryProtocol`/`TCompactProtocol` to *deserialize from a file or queue* (e.g., persisted Thrift records) crosses the same boundary as the network case: the bytes are untrusted, the calling code is trusted. Most runtimes do not enforce a `maxMessageSize` on a non-network transport (`TMemoryBuffer`, `TFileTransport`), so the resource-bound guarantee is weaker here. *(maintainer — Q15.)*

### Reachability preconditions per family

For each row in the §2 component-family table, the test a triager applies before deciding "is this in model":

- **IDL compiler** — a finding is in-model only if reachable from a `.thrift` file the **build pipeline** consumes from a trusted source. Bugs reachable from adversary-supplied `.thrift` files are `OUT-OF-MODEL: adversary-not-in-scope` per §3 #8. *(maintainer — Q16.)*
- **C++ / c_glib / Java / Python / Go / Rust / other runtimes** — a finding in a `T*Protocol::read*` method is in-model only if reachable from a byte stream that crosses a Boundary-1 / Boundary-2 / Boundary-3 transition. A finding reachable only from `T*Protocol::write*` (a *serializer* fed in-process structs) is in-model only if the input struct can be attacker-controlled (e.g., a server echoing user-supplied data back); pure writer-side bugs on trusted in-process input are typically `OUT-OF-MODEL: trusted-input`. *(maintainer — Q17.)*
- **Generated code** — `struct.read()` is in-scope under the same condition as protocol reads. `struct.write()` is in-scope only when fed attacker-influenced fields. The processor dispatch (`process_<methodname>` in most bindings) is in-scope as the path from wire → handler.
- **SSL/SASL/AccessManager** — in-scope as part of the network surface. A finding in the C++ `AccessManager::verify` callback contract is in-model when the documented verify-decision protocol is misimplemented or has a logic flaw that lets a denied cert appear allowed *(documented — `lib/cpp/README.md` §AccessManager)*; failing to *call* an AccessManager is the operator's responsibility, not Thrift's.

### Data not crossing a boundary

Several internal paths are not trust transitions and do not generate findings:

- Generated-code → runtime interface calls (`iprot->readI32(&val)`). Both sides ship together; both are Thrift.
- `TMultiplexedProtocol` dispatch by service-name prefix. The lookup is among services the application has registered with the multiplexed processor; the service-name string came from the wire but the *set of valid choices* is bounded by the application's registration.
- Internal allocator interactions in non-C++ runtimes (GC).

---

## §5 Assumptions about the environment

Thrift is a polyglot library; "the environment" varies per binding. Common assumptions, then per-runtime specifics.

### Cross-runtime assumptions

- **The host OS provides working sockets, TLS-capable cryptography, and a working allocator.** Thrift does not implement TCP, TLS handshakes, or memory allocation itself. *(maintainer — Q18.)*
- **Byte order on integer types matches the spec.** Binary protocol uses network byte order *(documented — `doc/specs/thrift-binary-protocol.md` §Integer encoding)*; Compact protocol uses ZigZag varints (LSB first) *(documented — `doc/specs/thrift-compact-protocol.md`)*. The "CPP version has the option to use the binary protocol with little endian order" *(documented — same spec)*; this is an off-spec interop mode and must not be enabled when talking to non-C++ peers. *(maintainer — Q19 — confirm this is operator's problem.)*
- **The `double` IEEE 754 layout is conformant.** *(documented — both spec files.)*
- **No assumption is made that 32-bit `int32`/`int64` arithmetic does not overflow.** History suggests the opposite: `THRIFT-5716` is a `TMemoryBuffer` `uint32_t` overflow that shrank the buffer; `THRIFT-3821` is a `TMemoryBuffer` resize overflow; `THRIFT-5812` is a Rust server `with_capacity` overflow *(all documented — `CHANGES.md`)*. The model should state that **integer overflow on any value derived from wire-supplied lengths is a security-relevant bug** — most existing CVEs and CVE-class JIRAs in Thrift are exactly this class. *(maintainer — Q20.)*
- **Signal-safety is not provided.** The C++ runtime warns about `SIGPIPE` and recommends the application ignore it *(documented — `lib/cpp/README.md` §SIGPIPE signal)*; this is a documented `do-the-following` for the operator, not a guarantee Thrift provides.

### C++ runtime (`lib/cpp/`)

- **C++11 minimum.** *(documented — `lib/cpp/README.md` §Dependencies.)*
- **OpenSSL, pthreads, `librt`** for `libthrift`. *(documented — same.)*
- **`libevent`** for `libthriftnb` (the nonblocking server). *(documented — same.)*
- **The PRNG seed for TLS is "key to the application security"** and `TSSLSocketFactory::randomize()` calls OpenSSL's `RAND_poll()` "when OpenSSL library is first initialized" — operators "should override [randomize] if it's not strong enough for you" *(documented — `lib/cpp/README.md` §TSSLSocketFactory::randomize)*. The implication: by default Thrift trusts OpenSSL's `RAND_poll()` seeding to be sufficient.
- **Windows ≥ Windows 7** for the C++ runtime; XP/Vista support was dropped after 0.12.0 *(documented — `lib/cpp/README.md` §Windows version compatibility and §Breaking Changes 1.0.0)*.
- **TSSLSocketFactory lifetime** *(documented — `lib/cpp/README.md` §Breaking Changes 0.11.0 / THRIFT-4164)*: the factory must outlive every `TSSLSocket` it creates, or OpenSSL is torn down too early. This is an embedded-API contract; violating it leads to an assertion or crash (changed from silent undefined behavior in 0.11.0).
- **Named Pipe transport (`TPipe`/`TPipeServer`) is Windows-only and supports 1:1 only.** *(documented — `lib/cpp/README.md` §Named Pipes.)*
- **`TSSLSocket` only supports blocking-mode I/O** and only `TSimpleServer`, `TThreadedServer`, `TThreadPoolServer` *(documented — `lib/cpp/README.md` §Thrift/SSL Scope)*. Combining TLS with the nonblocking server is not supported — see also THRIFT-5515 in CHANGES.md ("oneway requests can stuck in TNonblockingServer with TSSLSocket").

### Java runtime (`lib/java/`)

- **JVM with `javax.security.sasl` available.** *(maintainer — Q21.)*
- **The JSSE provider supplies the TLS implementation.** *(maintainer — Q22.)*
- **Past CVE (CVE-2018-1320, THRIFT-4506):** an `assert` in SASL code was ignored in release builds, allowing an authentication bypass. The fix removed the load-bearing assert; this is a *(documented)* lesson that release-mode JVM semantics must not be assumed.

### Python runtime (`lib/py/`)

- **Standard library `ssl` for TLS.** *(maintainer — Q23.)*
- **`TNonblockingServer` and SSL did not work** until THRIFT-3938 (in CHANGES.md). Some `T*Server` × TLS combinations have historically been broken.
- **Python ≥ 2.7.9** required for usable `TSSLSocket` *(documented — THRIFT-4026)*. Modern Thrift Python presumably targets Python 3 only; the exact floor is *(maintainer — Q24)*.

### Go runtime (`lib/go/`)

- **Officially supports the latest two Go releases** *(documented — `lib/go/README.md` §Suppored Go releases)*.
- **`thrift.ServerConnectivityCheckInterval`** defaults to roughly 1ms granularity client-disconnect detection; setting it to `0` disables the feature; setting it too low (e.g., 1ms) can cause excessive CPU due to a Go runtime bug *(documented — `lib/go/README.md`)*.
- **`thrift.ServerStopTimeout`** controls graceful shutdown; default is `<=0` (wait forever for clients) *(documented — same)*.
- **Past CVE-class:** THRIFT-5469 (stack overflow on skip of nested map values), THRIFT-5221/5255 (stack overflow when reading buffer / framed transport).

### Rust runtime (`lib/rs/`)

- **Rust 1.65+ since 0.14.0** *(documented — `lib/rs/README.md` §Thrift 0.14.0 / THRIFT-5158)*.
- **Past CVE-class:** THRIFT-5812 (capacity overflow in Rust server).

### What Thrift does *not* do to its host

This is the negative-claim inventory and is the highest-priority confirmation target *(per `output-structure.md` §5)*. Every item below is *(inferred)* and routed to §14 wave 2.

- **Does not install signal handlers** by default *(maintainer — Q25)*. The C++ `SIGPIPE` guidance asks the *application* to handle it.
- **Does not spawn child processes** at runtime in any binding *(maintainer — Q26)*. The IDL compiler is a build-time tool and is not part of runtime.
- **Does not read environment variables** at runtime *(maintainer — Q27)*. (Build tooling reads `BOOST_ROOT`, `OPENSSL_ROOT_DIR`, etc., but those are build-time.)
- **Does not write to global state outside the explicit `*Factory` singletons** (`TSSLSocketFactory` is documented as having process-wide OpenSSL initialization side-effects). *(maintainer — Q28.)*
- **Does not write to `stdout`/`stderr`** in normal operation; exception logging uses per-binding logger facades. *(maintainer — Q29.)*
- **Does not open files** other than what the application explicitly hands to `TFileTransport` (or per-binding equivalents). *(maintainer — Q30.)*
- **Does not touch the locale or FPU state.** *(maintainer — Q31.)*

The fragility of this list is real: a single binding that, say, calls `OpenSSL_add_all_algorithms()` on first use is a process-wide side effect a downstream FFI consumer will notice. The PMC's confirmation here defines what "embedding Thrift" actually costs.

---

## §5a Build-time and configuration variants

Thrift has more configuration surface than most libraries because it is 28 bindings, each with its own build system, plus a code generator with per-language flags.

### Runtime-affecting compile-time and link-time choices

| Knob / flag | Family | Default | Effect on model | Maintainer stance |
| --- | --- | --- | --- | --- |
| `THRIFT_TUUID_SUPPORT_BOOST_UUID` | C++ | off | Enables implicit construction of `TUuid` from `boost::uuids::uuid`. Implicit constructor risks accidental conversions. *(documented — `lib/cpp/README.md` §TUuid and boost::uuids::uuid)* | *(maintainer — Q32)* |
| `THRIFT_TUUID_BOOST_CONSTRUCTOR_EXPLICIT` | C++ | off | When the above is on, makes the constructor explicit. *(documented — same)* | *(maintainer — Q32)* |
| `FORCE_BOOST_SMART_PTR` | C++ | off | Forces Thrift to use `boost::smart_ptr` in headers instead of `std::shared_ptr`. *(documented — `lib/cpp/README.md` §Breaking Changes 0.11.0 / THRIFT-2221)* | *(maintainer — Q33)* |
| `HAVE_CONFIG_H` + `windows/config.h` | C++ Windows | on for Windows builds | Required for VS-based projects. *(documented — `lib/cpp/README.md` Windows section)* | not security-affecting |
| `WITH_SHARED_LIBS` / `WITH_STATIC_LIBS` | C++ CMake | deprecated | Replaced by standard `BUILD_SHARED_LIBS` *(documented — Breaking Changes 1.0.0 / THRIFT-4732)* | not security-affecting |
| Compiler-level `-DNDEBUG` (any C++ build) | C++ | depends on build | **CVE-2018-1320 / THRIFT-4506 lesson:** an `assert` may be compiled out in release builds. The model must say: no security check may rely on `assert`. *(documented as a lesson via CHANGES.md)* | *(maintainer — Q34)* |
| Java `cloverEnabled` | Java | off | Coverage tooling only. *(documented — `lib/java/README.md`)* | not security-affecting |
| Go `thrift.ServerConnectivityCheckInterval` | Go | non-zero (default-on) | When non-zero, detects client disconnects and propagates `ErrAbandonRequest`. Setting to `0` disables. Setting too low (e.g. 1ms) causes CPU spin per a known Go-runtime bug. *(documented — `lib/go/README.md`)* | *(maintainer — Q35)* |
| Go `thrift.ServerStopTimeout` | Go | `0` (wait forever) | When `<=0`, `Stop()` waits for all client connections to drain — can hang indefinitely if peers do not close. *(documented — `lib/go/README.md`)* | **Insecure-default candidate** — wave-1 question. *(maintainer — Q36)* |
| C++ binary protocol little-endian mode | C++ | off | Allows the C++ binary protocol to use little-endian integer encoding for a performance win. **Off-spec when peers are non-C++.** *(documented — `doc/specs/thrift-binary-protocol.md`)* | *(maintainer — Q37)* |
| `TBinaryProtocol::setStringSizeLimit(int32_t)` / `setContainerSizeLimit(int32_t)` | most runtimes | **No limit** (i.e., `INT32_MAX`) | The single most consequential default in the entire model. *(documented — `doc/specs/thrift-binary-protocol.md` §List and Set: "by default, there is no limit (meaning the limit is the maximum int32 value: 2147483647)"; §Map: identical wording.)* | **Insecure-default — wave-1 question.** *(maintainer — Q38)* |
| `TFramedTransport` / `THeaderTransport` `setMaxFrameSize` | most runtimes | binding-dependent; commonly 16 MiB | Caps the size of a single framed message. *(maintainer — Q39)* | *(maintainer — Q39)* |
| `TConfiguration::setMaxMessageSize` (added later) | most runtimes | binding-dependent | Newer cross-binding cap on total bytes per message. *(maintainer — Q40)* | *(maintainer — Q40)* |
| SASL mechanism choices (PLAIN / GSSAPI / DIGEST-MD5 / etc.) | Java + others | not enforced | Thrift allows the application to pick; weak mechs (PLAIN over a non-TLS transport, DIGEST-MD5 — see THRIFT-5926) are not refused. *(maintainer — Q41)* | *(maintainer — Q41)* |
| TLS version floor | C++/Java/Python/Go/Rust/etc. | binding + dependency-version dependent | THRIFT-4084 hardened SSLv3 negotiation; THRIFT-5743 adds TLS 1.3; THRIFT-5876 adds Delphi WinHTTP TLS 1.3. *(documented — CHANGES.md.)* No documented floor across all bindings. | *(maintainer — Q42)* |
| `AccessManager` registration on `TSSLSocketFactory` | C++ | **not set** (only basic cert-chain validation) | Without an `AccessManager`, no host-name verification beyond OpenSSL chain-of-trust. *(documented — `lib/cpp/README.md` §AccessManager.)* | **Insecure-default — wave-1 question.** *(maintainer — Q43)* |
| Strict vs old-style Binary message encoding | All bindings | both accepted on read | A server using strict mode rejects old; a server in default mode accepts both *(documented — `doc/specs/thrift-binary-protocol.md` §Message)*. | *(maintainer — Q44)* |

### Code-generator flags (`thrift -gen <lang>:<options>`)

The IDL compiler exposes per-generator options (`thrift -gen java:beans,private-members`, `thrift -gen cpp:no_skeleton`, etc.). Many of these are pure ergonomics; a few flip wire-affecting or struct-shape-affecting behavior (e.g., `thrift -gen java:fullcamel`). The list is in each generator's `t_<lang>_generator.cc`. *(maintainer — Q45 — generator-option inventory needed.)*

### Insecure-default summary (the §13 routing question)

Three knobs above ship with the **less-secure** value as their default:

- **Container/string size limit** = `INT32_MAX` *(documented)* — a deserializer reading a hostile message that declares "list of 2 billion `i32`" will attempt to allocate 8 GiB before discovering anything is wrong.
- **`AccessManager`** = unset *(documented)* — C++ TLS does cert-chain validation but **no host-name verification by default** unless the application registers an `AccessManager`. THRIFT-5343 ("`TTlsSocketTransport` does not resolve IPv4 addresses or validate hostnames correctly") is a *(documented)* reminder this is an active landmine.
- **Go `ServerStopTimeout`** = `0` (wait forever) *(documented)* — not a network-attack DoS but an availability hazard on planned restart.

Per the SKILL `output-structure.md` §5a "insecure-default" rule, the PMC must rule on each: is the default the supported production posture (a report against it is `VALID`), or a dev-convenience that operators are required to flip (`OUT-OF-MODEL: non-default-build`, and the requirement appears in §10)? Resolution drives §8/§9/§10/§11a simultaneously and **must** land in wave 1.

---

## §6 Assumptions about inputs

Thrift accepts inputs from three distinct directions:

1. **Bytes off a transport** (network or in-process buffer) consumed by `TProtocol::read*` and then by `generated_struct.read()` and `processor.process()`.
2. **In-process values** passed to `TProtocol::write*` and `generated_struct.write()` (the serializer direction).
3. **`.thrift` IDL files** consumed by the compiler at build time.

### Per-parameter trust table

For RPC services, the "function" column is the protocol/processor entry point or the wire message kind, and "parameter" is the field on the wire. Rows are grouped by component family; for brevity, only canonical examples per binding are shown — all bindings inherit the row's trust posture unless §14 questions resolve a per-binding override.

| Component / function | Parameter | Attacker-controllable? | Caller must enforce |
| --- | --- | --- | --- |
| `TBinaryProtocol::readMessageBegin` | message header (version, type, name, seqid) | **yes** (wire) | nothing — protocol must defend itself |
| `TBinaryProtocol::readMessageBegin` | `name` length (`int32`) | **yes** | bind to `maxMessageSize` if set; otherwise unbounded |
| `TBinaryProtocol::readFieldBegin` | `field-type` (`int8`) | **yes** | reject unknown types or skip safely |
| `TBinaryProtocol::readFieldBegin` | `field-id` (`int16`) | **yes** | nothing — only used as a dispatch key into generated code |
| `TBinaryProtocol::readListBegin` / `readSetBegin` | element-type (`int8`), `size` (`int32`) | **yes** | enforce `containerSizeLimit` *(operator must set; default unlimited)* |
| `TBinaryProtocol::readMapBegin` | key-type, value-type, `size` (`int32`) | **yes** | enforce `containerSizeLimit` |
| `TBinaryProtocol::readBinary` / `readString` | `size` (`int32`), bytes | **yes** | enforce `stringSizeLimit` |
| `TBinaryProtocol::readI8/I16/I32/I64/Double/Bool/Uuid` | the value | **yes**, but bounded by fixed-width encoding | nothing — width is fixed |
| `TCompactProtocol::readVarint*` | varint bytes | **yes** | reject overlong encodings / cap at 10 bytes for `int64` |
| `TCompactProtocol::readListBegin` | element-type nibble + size (varint, or full `int32` if size ≥ 15) | **yes** | enforce `containerSizeLimit` |
| `TJSONProtocol::*` | JSON text | **yes** | UTF-8 validity, JSON nesting depth |
| `TFramedTransport::readFrame` | frame size (`int32`) | **yes** | enforce `maxFrameSize` |
| `THeaderTransport::readFrame` | header fields + frame size | **yes** | enforce `maxFrameSize`; reject unknown header transforms |
| `TZlibTransport::read` | compressed bytes | **yes** | enforce decompressed-size cap (no built-in; downstream concern — see §9 "decompression bomb") |
| `TSocket::read` | bytes | **yes** | nothing — passes through |
| `TSSLSocket::read` | bytes (post-decrypt) | **yes** | TLS layer authenticates the channel; bytes inside the channel are still untrusted application data |
| `TSSLSocket` handshake | peer certificate | **yes** | operator must register `AccessManager` for host-name + custom checks *(documented — `lib/cpp/README.md`)*; cert-chain validation is automatic via OpenSSL |
| `TSSLSocketFactory::loadCertificate` | path to cert/key file | **no — trusted caller string** | sanitize before reaching this API |
| `TSaslClientTransport::sendSaslMessage` | SASL token | **yes** in both directions | mechanism-specific token validation (see THRIFT-5926 DIGEST-MD5 crash) |
| Generated `processor.process_<method>(iprot, oprot, seqid)` | method-name string from wire | **yes** | dispatch table must reject unknown methods cleanly (no crash) |
| Generated `Struct.read(iprot)` | declared field-id, declared field-type | **yes** | type-mismatch handling (per spec, "default Java impl has undefined behavior" *(documented — `doc/specs/thrift-binary-protocol.md` §Struct)* — this is itself a model gap; see §14 Q46) |
| Generated `Struct.read(iprot)` | recursive nesting of structs / containers | **yes** | enforce recursion-depth cap (recurring CVE — THRIFT-5469 Go skip stack overflow, THRIFT-5221/5255 buffer read stack overflow) |
| `TMultiplexedProtocol` dispatch | service-name prefix on the wire | **yes** | application's registered service map bounds the choice |
| `THttpServer::readMessageBegin` | HTTP method, headers, body | **yes** | the HTTP layer must reject malformed requests; Thrift trusts the body as a Thrift message |
| IDL compiler `Parser` | `.thrift` file bytes | **no — trusted build-time input** | application developer chooses the file |
| IDL compiler `-gen <lang>:<options>` | generator options | **no — trusted build-time string** | application developer chooses options |
| IDL compiler `-I <path>` | include paths | **no — trusted build-time string** | application developer chooses include resolution |

### Size, shape, rate

- **`int32` sizes everywhere.** The Binary protocol spec mandates `int32` size fields for binary, list, set, map; all are 32-bit signed. The spec explicitly notes "by default, there is no limit (meaning the limit is the maximum int32 value: 2147483647)" for lists/sets and for maps *(documented)*. This means a hostile message can declare a 2-billion-element container that the runtime will attempt to honor unless the operator has set a `containerSizeLimit`.
- **Streaming vs buffered.** `TFramedTransport` buffers a whole frame before deserializing; `TBufferedTransport` buffers reads/writes; `TSocket` is unbuffered. The frame-size cap (if set) is the upper bound on frame-buffered consumption. *(maintainer — Q47.)*
- **Memory-mapped.** Not a documented mode. *(maintainer — Q48.)*
- **Rate.** Thrift does no rate-limiting; an attacker may send as many messages as the transport allows. *(maintainer — Q49.)*

### Cross-binding parity assumption

Every row above is the wire-protocol contract. The **actual enforcement of size caps is per-binding**: not all 28 bindings implement `setContainerSizeLimit` / `setMaxMessageSize` / `TConfiguration` the same way. The model assumes (Q50) that the C++/Java/Python/Go/Rust bindings are the reference set; other bindings may lag. A finding "binding X does not honor `maxMessageSize`" is `VALID` against the cross-binding parity claim if confirmed by the PMC; otherwise it is `MODEL-GAP`.

---

## §7 Adversary model

Thrift has multiple in-scope adversaries because it sits at both ends of the wire and is also a build-time tool.

### Primary adversary — the wire peer

The single highest-volume adversary is the **untrusted peer on the other end of the transport**.

- **Capability.** Sends arbitrary bytes — including bytes that do not match the IDL schema. May open many concurrent connections (against `T*Server`). May refuse to read responses (against a Thrift client). May close mid-frame, send oversized frames, send mismatched type tags, send deeply nested containers, replay messages, etc.
- **What they cannot do.** Influence the in-process struct definitions, the generator output, the compiled code, the application handler logic, or the operator's TLS material.
- **Goal.** Crash the runtime (segfault in C++/c_glib; uncaught exception in Java/Python/Go/Rust); exhaust memory; exhaust CPU; trigger an out-of-bounds read/write; smuggle a malformed struct past the deserializer such that the handler operates on garbage; or escape the deserializer into the handler with attacker-controlled state.

This adversary is **symmetric**: a Thrift client deserializing a server's response is exposed to a hostile server in exactly the same way.

### Secondary adversary — the authenticated-but-Byzantine peer

Where SASL, mTLS, or `AccessManager` is wired up, the peer is *authenticated* but their bytes are still untrusted at the deserializer level. The model treats them identically to the primary adversary; authentication only narrows *who* the adversary can be, not *what* they can send. *(maintainer — Q51.)*

### Out-of-scope adversaries

- **In-process attackers.** Anyone running in the same process as the Thrift runtime is the application; the model does not defend against them.
- **Local-host co-tenants.** No memory-encryption, no sidechannel resistance, no constant-time guarantee. A co-tenant who can read process memory or time CPU operations has won.
- **Attackers with control of the `.thrift` source.** A `.thrift` file fed to the compiler is a build input; treating the compiler as a sandbox would be a category mistake.
- **Attackers with control of the build environment.** Compromised C++ compiler, JDK, Go toolchain, Rust toolchain, OpenSSL distribution, JCE provider, etc., are out-of-model.
- **Attackers exploiting the application handler.** A SQL-injection bug in `handleListUsers(name)` is not Thrift's problem.
- **Side-channel attackers.** Timing, cache, power, RowHammer.
- **Attackers leveraging missing operator hardening.** Network exposure decisions, firewall rules, listener interface choices, log-forwarding bugs in the host application — all operator concerns.

### Adversary capabilities by transport

- **`TSocket`** — full wire control, no authentication, no encryption.
- **`TSSLSocket`** — wire-level encryption + server cert validation (chain only, unless `AccessManager` is wired); without `AccessManager` the host-name check is whatever OpenSSL does by default (which on many configurations is **nothing** unless the application sets `SSL_set_hostflags` / equivalent). *(maintainer — Q52.)* mTLS available if the operator configures client certs.
- **`TFramedTransport` over `TSocket`** — same as `TSocket` plus the framing layer adds a single declared size field (a DoS lever if uncapped).
- **`THttpServer`** — HTTP is plaintext unless wrapped; HTTP request smuggling, header injection, etc., are HTTP-layer issues; Thrift treats the body as a Thrift message.
- **`TSaslTransport`** — the SASL mechanism (PLAIN, GSSAPI, DIGEST-MD5, etc.) determines authentication strength; the wire is plaintext unless wrapped with TLS *(maintainer — Q53)*. Note CVE-2018-1320 (THRIFT-4506) was a SASL bypass via an `assert` that was elided in release builds.
- **`TPipe`/`TPipeServer`** (Windows) — local-machine adversary only; named-pipe ACLs are the host OS's responsibility.
- **`TZlibTransport`** — adds a decompression layer; an adversary can ship a zip-bomb (no built-in decompressed-size cap — see §9).

### Adversary against the compiler

Out-of-scope per §3 #8. For completeness: the compiler is a C++ program that parses a custom grammar via flex/bison; the usual generator-input class of bugs (recursive grammar consuming stack, large include chains exhausting memory) is possible, but the threat model deems the input trusted. A `.thrift` file from an untrusted source is the application developer's problem, not Thrift's.

---

## §8 Security properties the project provides

Each property below states: (1) the property and conditions; (2) the violation symptom a triager should expect; (3) the severity tier; (4) provenance.

### Memory and process-safety properties

**P1. Memory safety on conforming wire input — memory-safe runtimes (Java, Python, Go, Rust, JavaScript, Ruby, …).**
- *Condition:* the runtime's underlying language provides memory safety; protocol code does not invoke `unsafe` blocks (Rust) or `sun.misc.Unsafe` (Java) or `ctypes` (Python).
- *Violation symptom:* uncaught `panic`/`RuntimeException`/`PanicException`/`KeyError` in the runtime layer that escapes the read loop, *or* an out-of-bounds slice access masked by a runtime exception, *or* a stack-overflow in recursive descent (THRIFT-5469 class).
- *Severity:* high — DoS-class; not memory-corruption-class.
- *Provenance:* *(maintainer — Q54.)*

**P2. Memory safety on conforming wire input — memory-unsafe runtimes (C++, c_glib).**
- *Condition:* operator has set finite `containerSizeLimit` / `stringSizeLimit` / `maxFrameSize`; OpenSSL is correctly initialized; `TSSLSocketFactory` lifetime contract is observed; integer arithmetic on declared sizes does not overflow.
- *Violation symptom:* heap-corruption, stack-buffer write past end (THRIFT-5931 class), use-after-free, double-free.
- *Severity:* **critical** — these are CVE-worthy when reachable from the wire.
- *Provenance:* *(maintainer — Q55)*; the historical record (THRIFT-3821, THRIFT-5221, THRIFT-5255, THRIFT-5716, THRIFT-5931) shows the maintainers *do* treat these as security bugs *(documented — CHANGES.md)*.

### Wire-format properties

**P3. Round-trip fidelity across protocols.**
- *Condition:* a struct serialized by binding A using protocol P and deserialized by binding B using protocol P produces the same logical struct, for all canonical bindings and {Binary, Compact, JSON} protocols.
- *Violation symptom:* mismatch in field values, missing fields, or different default-vs-present interpretation.
- *Severity:* correctness, not security, **unless** the mismatch is between a security-relevant field (e.g., an auth token round-trips differently) — then it is downstream's problem to validate.
- *Provenance:* *(documented — `FUZZING.md` §Goals "Serialization round-trip correctness")* — round-trip fuzzers run continuously on OSS-Fuzz for every supported binding × protocol.

**P4. Wire-format conformance to the published specs.**
- *Condition:* the runtime emits and accepts bytes per `doc/specs/thrift-binary-protocol.md`, `…-compact-protocol.md`, `…-json-protocol.md`, `…-rpc.md`.
- *Violation symptom:* a peer that follows the spec cannot interop.
- *Severity:* correctness; rarely security except for the C++ little-endian binary mode (off-spec; do not enable when talking to non-C++ peers).
- *Provenance:* *(documented — the spec files; FUZZING.md round-trip fuzzers exercise this.)*

**P5. Unknown-field tolerance.**
- *Condition:* a deserializer encountering a field-id it does not have a slot for skips that field cleanly.
- *Violation symptom:* deserializer aborts or mis-attributes the skipped field's payload to a subsequent field.
- *Severity:* DoS or correctness, depending on call site.
- *Provenance:* *(documented — `doc/specs/thrift-binary-protocol.md` §Struct: "possible to handle unknown fields while decoding; these are simply ignored".)* Note: this property is the source of the THRIFT-5469-class stack-overflow bug — recursive `skip` of a deeply nested unknown structure consumed stack.

### Concurrency properties

**P6. Per-connection thread safety in `T*Server`.**
- *Condition:* each connection runs on its own thread/goroutine/coroutine; the application handler is responsible for synchronizing shared state.
- *Violation symptom:* data corruption in `T*Server` state itself (not the handler).
- *Severity:* correctness, with security implications if state corruption leaks data across connections.
- *Provenance:* *(maintainer — Q56)*; documented per-binding for some servers (e.g., `TSimpleServer` is single-threaded).

### TLS / transport-security properties (when enabled)

**P7. TLS confidentiality and integrity when `TSSLSocket` is used and the underlying TLS library is correctly configured.**
- *Condition:* operator configures TLS version floor, cipher suites, and certificate trust per their environment; OpenSSL/JSSE/etc. are unpatched-CVE-free.
- *Violation symptom:* downgrade, MITM, ciphertext decryption, replay.
- *Severity:* critical when applicable.
- *Provenance:* *(maintainer — Q57)* — Thrift wraps the underlying TLS library; the property is theirs, Thrift's job is "not to break it" (e.g., don't accept SSLv3 by default — THRIFT-4084).

**P8. Server certificate chain validation.**
- *Condition:* `TSSLSocket` clients validate the server cert chain via the configured trust store.
- *Violation symptom:* a forged/self-signed cert is accepted.
- *Severity:* critical.
- *Provenance:* *(maintainer — Q58)* — implied by the AccessManager design *(documented — `lib/cpp/README.md`)*; not stated as a guarantee anywhere.

**P9. Server host-name verification — ONLY when `AccessManager` is registered.**
- *Condition:* C++ — only with explicit `AccessManager`. Other bindings — varies; THRIFT-5343 documents that Java's `TTlsSocketTransport` did not validate hostnames correctly historically.
- *Violation symptom:* a cert valid for `evil.example` is accepted when connecting to `bank.example`.
- *Severity:* critical when applicable.
- *Provenance:* *(documented — `lib/cpp/README.md` §AccessManager; CHANGES.md THRIFT-5343)* — explicitly **not** the default.

### Resource-bound property

**P10. Bounded memory and CPU per message — ONLY when size caps are configured.**
- *Condition:* operator has set `containerSizeLimit`, `stringSizeLimit`, `maxFrameSize`, and (where available) `maxMessageSize`. Recursion-depth caps are enforced in bindings that have them.
- *Violation symptom:* OOM, swap thrash, CPU spin, stack overflow, hang.
- *Severity:* high (availability).
- *Provenance:* *(documented — spec files state the defaults are unlimited; CHANGES.md shows recurring fixes for cases where the cap was missing or bypassable.)*
- **Threshold:** the model needs the PMC to pin a categorical line — *(maintainer — Q59)* — proposed: "super-linear memory or CPU in the *declared* size of any single field, given caps are set, is a bug; constant-factor blowup against caps is not. A hang on streaming input with no cap set is the operator's choice, not a bug." Without ratification this remains the most contested triage category.

### Compiler property

**P11. Deterministic, hermetic codegen.**
- *Condition:* the IDL compiler given identical `.thrift` files + identical generator flags emits byte-identical generated code across runs and platforms.
- *Violation symptom:* non-reproducible builds, generated code differing between runs.
- *Severity:* correctness; rare security implications.
- *Provenance:* *(maintainer — Q60.)*

### Properties tracked indirectly via fuzzing

The OSS-Fuzz integration *(documented — `FUZZING.md`)* exercises the deserializer + round-trip path in 9 bindings (Go, c_glib partial, C++, Java/JVM, JavaScript, Python, Ruby, Rust, netstd). Any property the fuzzers test is implicitly claimed by the project: **deserializing arbitrary bytes does not crash the runtime**. A finding from a fuzzer corpus is therefore `VALID` by default, modulo the resource-bound caveat in P10 (a "test case takes 90 seconds" finding on an unbounded read is `BY-DESIGN: property-disclaimed` per §9).

---

## §9 Security properties the project does *not* provide

This is the section that defines what *won't* be a bug. Each item is paired with the §13 disposition a matching report would receive.

### Disclaimed properties (downstream's job, not Thrift's)

**D1. No authentication of the *peer's bytes* beyond what the transport provides.** `TSocket` is unauthenticated. `TSSLSocket` authenticates the *channel* (and, with `AccessManager`, the *peer identity*); it does not authenticate that the bytes within the channel are well-formed against the application's policy. → `BY-DESIGN: property-disclaimed` for "we expected Thrift to reject bad messages from authenticated peers." *(documented — `lib/cpp/README.md`.)*

**D2. No application-layer authorization.** A peer that successfully completes TLS + SASL handshake is authorized to invoke any service method exposed via the processor. RBAC, per-method ACLs, scopes, rate-limits — all the application's job. *(maintainer — Q61.)*

**D3. No defense against unbounded resource consumption when caps are not configured.** Default container/string size limit is `INT32_MAX` *(documented)*. An operator who runs an unconfigured `T*Server` on a public socket is exposed to "list of 2 billion `i32`" attacks; this is *(maintainer — Q62)* either `BY-DESIGN: property-disclaimed` (the default is dev-only and operators must flip it; the requirement appears in §10) or `VALID` (the default is the supported posture and the bug is in the default itself). Wave-1 question.

**D4. No defense against decompression bombs in `TZlibTransport`.** The transport has no documented decompressed-size cap. An attacker can ship a small compressed payload that expands to many GiB. *(maintainer — Q63.)* Standard "compression-oracle / compression-bomb" disclaimer for any compression layer (see Well-known attack classes below).

**D5. No defense against XML-class / JSON-class parser bombs in `TJSONProtocol`.** Deeply nested JSON arrays/objects can drive recursive parse paths; the model does not promise a recursion-depth cap unless the binding documents one. *(maintainer — Q64.)*

**D6. No constant-time guarantees.** No comparison in Thrift is constant-time; do not use Thrift-deserialized structs as the substrate for HMAC verification, token comparison, or any other constant-time operation. *(maintainer — Q65.)*

**D7. No anti-replay.** Sequence IDs in messages (`seqid`) are sequence numbers for matching responses to requests *(documented — `doc/specs/thrift-binary-protocol.md` §Message)*, not nonces; they do not prevent an attacker from re-sending a captured message. *(maintainer — Q66.)*

**D8. No confidentiality without TLS.** `TSocket`, `THttpServer` (without HTTPS), `TPipe`, `TSaslTransport` (without TLS wrap) all transmit application data in cleartext.

**D9. No defense against denial-of-service via socket pressure.** Slowloris-style slow-read, connection floods, half-open-connection exhaustion. The `T*Server` accepts; the OS provides backlog. Operator must front with a load balancer or use the OS's listen-backlog tuning. *(maintainer — Q67.)*

**D10. No safe behavior when type-tag and field-id disagree with the IDL.** Per the binary spec, "The default Java implementation (Apache Thrift 0.9.1) has undefined behavior when it tries to decode a field that has another field-type than what is expected. Theoretically, this could be detected at the cost of some additional checking. Other implementation may perform this check and then either ignore the field, or return a protocol exception." *(documented — `doc/specs/thrift-binary-protocol.md` §Struct.)* This is **per-binding** behavior; the model does not guarantee uniformity. Cross-binding behavioral divergence on malformed type-tag is `BY-DESIGN: property-disclaimed`.

**D11. No defense against CPU exhaustion by valid-but-pathological inputs.** A message that triggers many small allocations within configured caps, or a fan-out of nested-struct expansion, can cost CPU even under caps. *(maintainer — Q68.)*

**D12. No isolation between SASL/TLS state and other connections.** SASL/TLS contexts are per-connection; a bug in `TSaslTransport` state machine (cf. THRIFT-5926 DIGEST-MD5 None-initial-response crash) can crash that connection, but cross-connection state leaks are *(maintainer — Q69)* not modeled.

**D13. No defense against `TMultiplexedProtocol` service-name-prefix injection.** A peer choosing the service-name prefix can route between registered services freely; if the application registers a privileged and an unprivileged service on the same multiplexed processor, **the peer chooses which one to talk to**. *(maintainer — Q70.)*

**D14. No defense against the "off-spec C++ little-endian binary protocol" interop hazard.** If a C++ server is built with little-endian binary protocol enabled and a non-C++ client talks to it, the result is silent data corruption. *(documented — `doc/specs/thrift-binary-protocol.md`.)*

### False-friend properties

**F1. `TFramedTransport` looks like a "frame size sanity check" but is not a `maxMessageSize` cap by default.** It enforces *that the receiver buffers a whole frame before dispatching*, not *that the frame is within a sane size*. Without `setMaxFrameSize`, the frame size is the wire-declared `int32`. Triage of "you accepted a 2 GiB frame" depends on whether `setMaxFrameSize` was called.

**F2. `TBinaryProtocol`'s `version` field looks like a versioning safety net but is fixed-to-`1` *(documented)* and rejects messages without the version bit set when "strict mode" is enforced; the default mode accepts both formats.** Operators expecting strict-mode rejection get accept-both unless they enable strict mode.

**F3. `TSSLSocket` `accessmanager == null` does *not* mean "we will fall back to standard host-name verification".** It means "no host-name verification is performed beyond cert-chain trust". *(documented — `lib/cpp/README.md` §AccessManager.)* This is the C++-binding-specific landmine; other bindings' defaults vary (THRIFT-5343 shows the JVM binding had its own version of this for IPv4).

**F4. `TSocket::getPeerHost()` returns the *numeric IP* when reverse-DNS fails, **not** the connecting host name.** *(documented — `lib/cpp/README.md` §AccessManager.)* Code that uses `getPeerHost()` as if it were a verified host name is making an unwarranted assumption.

**F5. SASL/PLAIN looks like authentication but, over an unencrypted transport, sends credentials in cleartext.** *(maintainer — Q71.)*

**F6. The `seqid` looks like a session token / replay guard but is a sequence number for response-matching.** *(see D7.)*

**F7. The Thrift `uuid` type looks like a unique identifier with cryptographic guarantees; it is just a 16-byte opaque blob with no required generation algorithm.** *(documented — `doc/specs/thrift-binary-protocol.md` §Universal unique identifier encoding.)* A peer can send `00000000-0000-0000-0000-000000000000` or any other value freely.

### Well-known attack classes left to the caller

- **Decompression bomb** against `TZlibTransport`. (See D4.)
- **Algorithmic-complexity / hash-collision DoS** against any binding's map-deserialization. *(maintainer — Q72.)* Map-keyed structs deserialize into the binding's native map; binding-level hash-flooding defenses (or lack thereof) apply.
- **Recursive-deserializer stack overflow.** Documented occurrences: THRIFT-5469 (Go), THRIFT-5221 / THRIFT-5255 (multiple). The model does not promise an explicit recursion-depth cap in every binding.
- **JSON-protocol depth bombs.** (See D5.) Standard parser-bomb class.
- **Slow-decoder DoS.** A peer sending many small partial reads forces buffer growth. (See D9.)
- **CPU exhaustion via container-of-container fanout** under configured caps. (See D11.)
- **TLS downgrade / weak ciphers / weak TLS versions** are the underlying library's posture; Thrift inherits them and provides no enforcement. (See D8 / P7.)
- **HTTP request smuggling** against `THttpServer`. Out of model.
- **`process_<methodname>` dispatch on unauthorized methods.** Application's authorization concern (D2).

---

## §10 Downstream responsibilities

A contract — not a how-to. These are the actions the operator and/or application developer must take for §5–§8 to hold.

### Application developer responsibilities

1. **Pick a binding the model says is in-scope.** C++, Java, Python, Go, Rust have the highest *(documented)* coverage. Other bindings are in-model by extension but with reduced confidence (§14 Q26–Q30).
2. **Pin a Thrift release.** A `.thrift` file plus a binding version is the unit a deployment commits to. Floating-version dependencies stretch the threat model across versions in ways the model cannot cover.
3. **Compile generated code with the security caps that the binding exposes.** For C++, this means using `TConfiguration` / `setContainerSizeLimit` / `setStringSizeLimit` / `setMaxFrameSize` in the protocol/transport constructors. Per-binding equivalents apply.
4. **Treat *all* deserialized struct fields as untrusted application data.** Even after Thrift's wire-level checks pass, every field came from the wire. Validate `string email` against an email regex; validate `i32 limit` against the application's allowed range; validate `list<UserId> selection` is non-empty and within the application's limits. (§9 D2.)
5. **Do not rely on `assert` for security checks.** (THRIFT-4506 lesson — CVE-2018-1320.)
6. **For `TMultiplexedProtocol`, do not register a privileged and an unprivileged service on the same processor without also enforcing peer-identity checks per request.** (§9 D13.)
7. **Do not enable the C++ little-endian binary mode** when peers are non-C++. (§9 D14.)
8. **Do not feed adversary-supplied `.thrift` files to the compiler.** (§3 #8.)

### Operator responsibilities

9. **Set finite container/string size limits and a `maxFrameSize`** before exposing a `T*Server` to anything untrusted. The defaults are unlimited. (§5a, §9 D3.) **This is the single highest-leverage operator action.**
10. **Set `maxMessageSize`** if the binding exposes `TConfiguration`.
11. **Register an `AccessManager` (C++) or its per-binding equivalent** if any peer trust decision depends on host-name verification or custom cert checks. (§5a, §9 F3.)
12. **Configure a sane TLS version floor** (TLS 1.2 minimum; 1.3 preferred where supported per THRIFT-5743/5876) and disable known-weak ciphers in the underlying library.
13. **Do not combine `TSSLSocket` with `TNonblockingServer` (C++).** Unsupported combination. *(documented — `lib/cpp/README.md` §Thrift/SSL Scope.)*
14. **Front public Thrift endpoints with a load balancer / reverse proxy** that handles slowloris, connection-rate limits, and source-IP rate limits. (§9 D9.)
15. **Hold `TSSLSocketFactory` for the lifetime of all `TSSLSocket`s it creates** (C++; THRIFT-4164).
16. **Ignore `SIGPIPE`** in C++ applications using `TSSLSocket`. *(documented — `lib/cpp/README.md` §SIGPIPE signal.)*
17. **Configure `thrift.ServerStopTimeout`** (Go) to a finite value if you do not want `Stop()` to hang waiting for clients. *(documented — `lib/go/README.md`.)*
18. **Refresh TLS material on the schedule the data lifetime demands.** Thrift does not rotate keys.
19. **Patch the underlying TLS library** on the cadence its own advisories require — OpenSSL/JSSE/etc. CVEs are not Thrift CVEs, but they affect Thrift deployments.
20. **Choose SASL mechanisms that fit the deployment.** PLAIN over an unencrypted transport sends credentials in cleartext (§9 F5).

### Both

21. **Re-read this threat model on every Thrift major-version bump.** §12 lists the changes that should trigger a revision; in lieu of confirmation, treat any `0.X.0 → 0.(X+1).0` as a re-read.

---

## §11 Known misuse patterns

These are the patterns the model warns about — uses that the API allows but the contract does not support.

**M1. Exposing a `T*Server` to the internet with default size caps.** The combination "default `containerSizeLimit = INT32_MAX`" + "public socket" + "no front-end limiter" is the most consequential misuse. Symptom: OOM the first time a curious researcher (or attacker) sends a malformed message declaring a 2 GiB string. Fix: §10 #9.

**M2. Using `seqid` as a session token, anti-replay nonce, or auth correlator.** It is a response-matching sequence number, not a security primitive (§9 D7, F6).

**M3. Using the Thrift `uuid` type as if it had cryptographic uniqueness or unpredictability.** It is opaque 16 bytes; the generator is whatever the application chose (§9 F7).

**M4. Building a security check on `assert`.** CVE-2018-1320 / THRIFT-4506: a release build elided the assert, the check was effectively gone. Fix: use unconditional `if` + raise/throw.

**M5. Calling `getPeerHost()` and trusting the return value as a host name.** It can be a numeric IP if reverse-DNS fails (§9 F4).

**M6. Using `TSSLSocket` in C++ without registering an `AccessManager` and assuming host-name verification.** OpenSSL's default does not host-name verify; without `AccessManager`, no host-name check happens (§9 F3, THRIFT-5343 — analogous Java issue).

**M7. Mixing `TSSLSocket` and `TNonblockingServer` (C++).** Explicitly unsupported (§10 #13); THRIFT-5515 documents the kind of bug this produces.

**M8. Using `TSocket` (cleartext) in production.** Cleartext transport between trust domains — the model does not defend against passive observers (§9 D8).

**M9. Setting `containerSizeLimit` / `stringSizeLimit` to a number large enough to OOM the process when reached.** A limit at `INT32_MAX/2` is no protection. The cap must reflect actual application maximums.

**M10. Using `TBinaryProtocol` with strict mode off when interoperating with an unknown set of peer implementations.** The old encoding is accepted, which means a peer using the old encoding looks like a legitimate message even if you intended to refuse it.

**M11. Using `TZlibTransport` over an untrusted transport without an external decompressed-size cap.** Decompression-bomb territory (§9 D4).

**M12. Treating an authenticated peer's bytes as schema-conformant.** Authentication says *who*; the deserializer says *whether the bytes parse*. They are independent (§9 D1).

**M13. Stacking `TMultiplexedProtocol` over services with different trust requirements without per-method authorization.** A peer chooses which registered service to call (§9 D13).

**M14. Building under the assumption that `lib/<lang>/` binding X enforces the same caps as binding Y.** Cross-binding enforcement parity is *(maintainer — Q50)*; do not assume.

**M15. Promoting `contrib/` code into production without separately threat-modeling it.** Out of scope per §3.

**M16. Using TLS material from `test/keys/`.** They are committed test keys, demonstrably public.

---

## §11a Known non-findings (recurring false positives)

These are the patterns a scanner, fuzzer, AI-assisted reviewer, or human reviewer is likely to flag against `apache/thrift` but that are **not** bugs given the model. Triagers can close each with the cited section.

**N1. "Container-size field is read without an explicit upper bound in `T*Protocol::readListBegin`/`readMapBegin`/`readSetBegin`/`readBinary`."** Default cap **is** unlimited per spec *(documented — `doc/specs/thrift-binary-protocol.md`)*; the operator is documented as responsible for setting `setContainerSizeLimit` / `setStringSizeLimit`. → `BY-DESIGN: property-disclaimed` (§9 D3) unless wave-1 Q38 reclassifies the default as `VALID`.

**N2. "Decompression in `TZlibTransport::read` has no size limit."** Zlib does not impose one; Thrift does not impose one. → `BY-DESIGN: property-disclaimed` (§9 D4).

**N3. "Field type mismatch is undefined behavior."** Per the spec, this is binding-specific and explicitly not guaranteed uniform *(documented — `doc/specs/thrift-binary-protocol.md` §Struct)*. → `BY-DESIGN: property-disclaimed` (§9 D10).

**N4. "Recursive descent in `T*Protocol::skip` for nested structs/containers has no depth check."** This is the THRIFT-5469 class. *Fixed instances* are `VALID` historical bugs; *new instances in bindings that haven't been hardened* are `VALID` (the project clearly treats them as bugs per CHANGES.md). A static-analysis hit on a binding that does have a depth check is `KNOWN-NON-FINDING`. *(documented via CHANGES.md.)*

**N5. "Hard-coded test certificates in `test/keys/`."** Out of scope per §3; `OUT-OF-MODEL: unsupported-component`.

**N6. "`assert` in test code or in `tutorial/`."** Out of scope per §3. → `OUT-OF-MODEL: unsupported-component`. (Note: `assert` for *security checks* in **runtime** code is `VALID` — CVE-2018-1320 — and the model distinguishes the two.)

**N7. "TLS hostname verification is not performed by `TSSLSocket` constructor."** That is the documented contract (§9 F3); host-name check requires `AccessManager`. → `BY-DESIGN: property-disclaimed` unless the *operator's documented requirement* in §10 #11 wasn't met.

**N8. "Cleartext credentials in PLAIN SASL over `TSocket`."** SASL PLAIN over an unencrypted transport is application configuration; Thrift does not refuse the combination. → `BY-DESIGN: property-disclaimed` (§9 F5).

**N9. "`TBinaryProtocol::readMessageBegin` accepts both strict and old encoding by default."** Documented spec behavior *(`doc/specs/thrift-binary-protocol.md` §Message)*. → `BY-DESIGN: property-disclaimed`.

**N10. "Reading a `string` calls `read()` with a size taken from the wire."** That is the protocol. The defense is the configured `stringSizeLimit`, not removing the read. → `KNOWN-NON-FINDING` when the cap-enforcement path is present.

**N11. "Generated code calls `iprot.readI32(&val)` and uses `val` without an immediate range check."** The IDL does not declare value ranges; range validation is the application handler's job (§9 D2). → `BY-DESIGN: property-disclaimed`.

**N12. "Use-after-free risk in `TSSLSocketFactory` teardown."** Documented contract: factory must outlive sockets *(`lib/cpp/README.md` §Breaking Changes 0.11.0 / THRIFT-4164)*. → `OUT-OF-MODEL: trusted-input` (this is an embedded-API contract on the application).

**N13. "`TSimpleServer` blocks on slow clients."** `TSimpleServer` is the simplest server (single-threaded by design); high-concurrency deployments should pick `TThreadedServer` / `TThreadPoolServer` / `TNonblockingServer`. → `BY-DESIGN: property-disclaimed`.

**N14. "Endianness mismatch when C++ little-endian binary mode is enabled and peer is non-C++."** Explicit operator decision (§5a, §9 D14). → `OUT-OF-MODEL: non-default-build`.

**N15. "`SIGPIPE` not handled inside the runtime."** Documented operator responsibility *(`lib/cpp/README.md` §SIGPIPE)*. → `BY-DESIGN: property-disclaimed`.

**N16. "Test, tutorial, or contrib code does <foo>."** Per §3, out of scope. → `OUT-OF-MODEL: unsupported-component`.

**N17. "AI scan reports `getPeerHost()` is used unsafely."** It can return numeric IP (§9 F4); whether that is unsafe depends on the application. If the application uses it as an input to authorization, that is an application bug. → `OUT-OF-MODEL: trusted-input` for Thrift's responsibility.

**N18. "Use of `MD5` / `SHA-1` inside binding X."** Thrift does not promise cryptographic strength of any hash it embeds for internal purposes (e.g., reflection name hashing). → `BY-DESIGN: property-disclaimed` unless the hash is on an externally trusted path.

**N19. "Go `ServerStopTimeout` defaults to 0 (wait forever)."** Documented (*`lib/go/README.md`*) and operator-tunable. → `BY-DESIGN: property-disclaimed` unless wave-1 Q36 reclassifies.

**N20. "Integer overflow in `len * sizeof(T)` in the C++ runtime."** This is the THRIFT-3821 / THRIFT-5716 / THRIFT-5812 class — these are *(documented)* bugs the project fixes when found. → New instances are `VALID`; old fixed ones are `KNOWN-NON-FINDING`.

### Fuzzing-scope seed (per `FUZZING.md`)

OSS-Fuzz currently covers: **Go, c_glib (partial), C++, Java/JVM, JavaScript, Python, Ruby, Rust, netstd (local only)** *(documented — `FUZZING.md`)*. For each binding × protocol the fuzz coverage exists, the following dispositions apply:

- **Deserializer fuzzer corpus crashes the runtime → `VALID`.** Memory-safety on fuzzed inputs is implicit in P1/P2.
- **Round-trip fuzzer detects mismatch → `VALID`** against P3.
- **Deserializer corpus takes > N seconds / allocates > M bytes within configured caps → `KNOWN-NON-FINDING`** against §9 D11 unless wave-1 Q59 nails down a categorical CPU/memory threshold for "DoS by valid-but-pathological inputs".
- **Deserializer corpus exhausts memory because no `stringSizeLimit` / `containerSizeLimit` is set in the fuzz harness → `KNOWN-NON-FINDING`** (harness configuration issue; the harness must set caps; or wave-1 Q38 reclassifies the default).
- **A binding *not* on the OSS-Fuzz list discovers a deserializer crash via local fuzzing → still `VALID`** (the model claims P1/P2 across all bindings, even ones the fuzzer infrastructure has not yet reached).

---

## §12 Conditions that would change this model

The model should be revisited when any of the following land:

1. **A new public API surface in any binding** — e.g., a new `T*Transport`, a new `T*Protocol`, a new `T*Server` shape, or a new SASL mechanism integration.
2. **A new input format** — adding a new on-wire protocol next to Binary/Compact/JSON/Multiplex would re-open §4/§6/§8/§9.
3. **A change in default for a §5a knob**, especially `containerSizeLimit`, `stringSizeLimit`, `maxFrameSize`, `maxMessageSize`, `AccessManager` registration, `ServerStopTimeout`. Any of these changing from "unlimited / unset / 0" to a finite default re-classifies many §11a entries as `VALID`.
4. **Promotion of `contrib/` code into a `lib/<lang>/` runtime.** Newly in-scope; needs the §6/§7/§8 walk.
5. **Adoption of a uniform `TConfiguration` across all 28 bindings.** Reduces §6 row-fragmentation.
6. **Mandatory minimum recursion-depth cap in any binding's `skip()` path.** Closes the THRIFT-5469 / 5221 / 5255 family of bugs categorically.
7. **A SECURITY.md being added to the repo or a per-project security page on the website.** That artifact will become higher-authority than this draft (§3.1a of the SKILL); this document is to be back-mapped to it.
8. **The Thrift PMC's pre-published threat-model or security-policy document is published.** Same disposition as 7.
9. **The IDL grammar changes** (e.g., adding `optional` semantics, a new base type, default-value semantics for containers). Changes shape of generated code and the spec.
10. **A new CVE class is reported that cannot be cleanly routed to §13.** Per `output-structure.md` §12, "evidence that the model is incomplete" is itself a trigger: the model gets revised, not the report.
11. **A binding gains or loses OSS-Fuzz coverage.** Affects what corpus-derived findings are `VALID` vs `KNOWN-NON-FINDING` per §11a.
12. **The `apache/thrift-website` scope decision is reversed.** If the website is brought back into scope, it has its own threat model.

Re-version this document alongside the project. A report against Thrift `0.X.Y` is triaged against this model as it stood at `0.X.Y`, **not** as it stands at `HEAD`.

---

## §13 Triage dispositions

The closed set of outcomes a vulnerability report, tool finding, or AI analysis can receive against this model. A finding that does not fit one of these is `MODEL-GAP` and triggers §12 #10.

| Disposition | Meaning | Licensed by |
| --- | --- | --- |
| `VALID` | Violates a property the project claims (§8 P1–P11), via an in-scope adversary (§7) and in-scope input (§6), in an in-scope component (§2). | §8, §6, §7 |
| `VALID-HARDENING` | No §8 property is violated, but the API makes a §11 misuse easy enough that the project elects to harden it (e.g., adding a `setContainerSizeLimit` overload that requires an explicit cap rather than defaulting unlimited). Reported privately; fixed at maintainer discretion; typically no CVE. | §11 |
| `OUT-OF-MODEL: trusted-input` | Requires attacker control of a parameter the model marks trusted: a `.thrift` source file, a generator-options string, a `loadCertificate` path, an in-process caller's struct fields. | §6 |
| `OUT-OF-MODEL: adversary-not-in-scope` | Requires an attacker capability the model excludes — in-process access, side-channel, supply-chain compromise, control of the build toolchain. | §7 |
| `OUT-OF-MODEL: unsupported-component` | Lands in `test/`, `tutorial/`, `contrib/`, `compiler/cpp/test/`, or other code §3 places out of scope. | §3 |
| `OUT-OF-MODEL: non-default-build` | Only manifests under a discouraged or non-default §5a flag — e.g., `THRIFT_TUUID_SUPPORT_BOOST_UUID` enabled, C++ little-endian binary mode enabled, `FORCE_BOOST_SMART_PTR` set. | §5a |
| `BY-DESIGN: property-disclaimed` | Concerns a property §9 explicitly does not provide — host-name verification without `AccessManager`, decompression-bomb defense, anti-replay, constant-time comparison, etc. | §9 |
| `KNOWN-NON-FINDING` | Matches one of the recurring false-positive patterns enumerated in §11a (N1–N20). | §11a |
| `MODEL-GAP` | Cannot be cleanly routed to any of the above. Triggers a revision per §12 — *do not* make an ad-hoc call on the finding. | §12 |

### Worked routing examples

- *"Static analyzer flags `TBinaryProtocol::readListBegin` reading a 32-bit length without bounds check."* → `BY-DESIGN: property-disclaimed` (§9 D3, §11a N1). The default is unlimited per spec.
- *"Fuzzer corpus crashes `lib/cpp` with an OOB write."* → `VALID` (§8 P2).
- *"Test harness leaks the private key under `test/keys/server.key`."* → `OUT-OF-MODEL: unsupported-component` (§11a N5).
- *"`AccessManager::verify` returns `ALLOW` when the cert is for `evil.example` but the client connected to `bank.example`."* → `VALID` (§8 P9 — if the `AccessManager` *is* registered and the contract is misimplemented).
- *"`TSSLSocket` accepts a cert for `evil.example` when no `AccessManager` is registered."* → `BY-DESIGN: property-disclaimed` (§9 F3, §11a N7).
- *"Recursive `skip` of nested unknown structs blows the stack."* → `VALID` (§8 P1 violation; §11a N4 — historical class).
- *"`thrift -gen java:bean` produces a struct that ignores `optional` semantics."* → likely `MODEL-GAP` (codegen correctness); routes to §12 #10 unless the PMC ratifies a P5-extension property.
- *"The IDL compiler crashes on a `.thrift` file containing 200 nested `include` directives."* → `OUT-OF-MODEL: trusted-input` (§3 #8).
- *"Decompression of a 100-byte zlib stream yields 4 GiB."* → `BY-DESIGN: property-disclaimed` (§9 D4, §11a N2).
- *"Go server hangs on `Stop()` because clients never disconnect."* → `BY-DESIGN: property-disclaimed` (§9 D9, §11a N19) — operator must set `ServerStopTimeout`.
- *"Binding X does not enforce `maxFrameSize` even when set."* → `VALID` (§8 P10 violation; §6 cross-binding parity).

---

## §14 Open questions for the maintainers

Every *(inferred)* tag in §1–§13 has a numbered match here. Each question states a **proposed answer** for the Thrift PMC to confirm, correct, or strike. Grouped into 7 waves of 3–11 questions; respond one wave at a time.

When a question is answered, promote the matching *(inferred)* tags in the body to *(maintainer)* and delete the question.

---

### Wave 1 — scope, adversary, and the insecure-default rulings (must answer first)

These reshape the rest of the model.

**Q1.** A peer sending bytes to a Thrift server is **untrusted by default**; the runtime must not assume the bytes match the IDL.

**Q2.** A Thrift **client** deserializing a server response is also exposed to untrusted bytes; the server-to-client direction is in the same threat model as client-to-server.

**Q3.** The folders `contrib/`, and `compiler/cpp/test/` are **out of model** — findings against them are `OUT-OF-MODEL: unsupported-component`. However, `test/` and `tutorial/`should be included to prevent promotion of insecure practice via bad example or test code.

**Q38.** **(Insecure-default — the headline question.)** 
A developer convenience that operators are documented as required to flip — meaning the same incident is `OUT-OF-MODEL: non-default-build`. §10 #9 carries the requirement. 

**Q43.** **(Insecure-default — TLS host-name.)** 
Per the documented contract. Potentially subject to change in future releases.

**Q36.** **(Insecure-default — Go `ServerStopTimeout`.)** 
Operators expected to set it. Documented, tested, no bug. The model should call this out as a hazard in §10.

---

### Wave 2 — what the runtime does (and does not do) to its host

These resolve the negative-claim inventory in §5, which is the highest-priority *(inferred)*-heavy block in the document.

**Q4.** 
Thrift does not ship a "secure-by-default RPC endpoint" — there is no Thrift binary that listens on a port. Embedding `T*Server` into an application is the only deployment mode, and the application owns the posture.

**Q5.** 
A `T*Server` runs the application handler in-process; there is no isolation between the deserializer and the handler; Thrift does not claim to be a sandbox.

**Q6.** 
Cryptographic guarantees from `TSSLSocket` / `TSaslTransport` are inherited from OpenSSL / JSSE / `crypto/tls` / `ssl` (etc.); Thrift's role is to wire them up correctly, not to be a crypto primitive.

**Q7.** 
Thrift validates **wire encoding** (type tags, declared sizes against caps); it does not validate **application semantics** of fields (email format, ID ranges, enum-value-in-set, foreign-key existence).

**Q8.** 
Attackers with control of the calling process are out of model.

**Q9.** 
Attackers with control of the `.thrift` source fed to the compiler are out of model — the IDL compiler is a developer tool, not a sandbox.

**Q10.** 
Side-channel attacks (timing, cache, power) against Thrift-deserialized data are out of model; Thrift provides no constant-time guarantees.

**Q11.** 
Raw socket-level DoS (slowloris, SYN flood, half-open exhaustion) is out of model — operator's job + load balancer.

**Q12.** 
None — everything in `contrib/` is unsupported.

**Q25.** 
No Thrift binding installs signal handlers at runtime.

**Q26.** 
No Thrift binding spawns child processes at runtime.

**Q27.** 
No Thrift binding reads environment variables at runtime (build tools do, but those are out of model).

**Q28.** 
Yes, there are a few others. Here's the full picture by binding:

---
C++ — TWinsockSingleton (Windows only)

lib/cpp/src/thrift/windows/TWinsockSingleton.cpp:37

Calls WSAStartup(MAKEWORD(2,2), ...) via std::call_once when any socket is used on Windows, and WSACleanup() on destruction. This is a genuine second process-wide library init
 — same category as the OpenSSL one, but Windows-only and gated behind call_once.

C++ — TOutput::instance() (all platforms)

lib/cpp/src/thrift/TOutput.h:52

A process-wide logging singleton (writes to stderr by default). The function pointer it holds is replaceable via setOutputFunction(), making it a globally mutable hook into
the process's error output. Less severe than OpenSSL init, but it is shared state.

C++ — SIGPIPE (not installed — deliberately avoided)

TSocket uses MSG_NOSIGNAL on Linux and SO_NOSIGPIPE on BSD/macOS — per-socket/per-call options. Thrift does not touch the process-wide SIGPIPE handler, which is the correct
design.

Yes, there are a few others. Here's the full picture by binding:

---
C++ — TWinsockSingleton (Windows only)

lib/cpp/src/thrift/windows/TWinsockSingleton.cpp:37

Calls WSAStartup(MAKEWORD(2,2), ...) via std::call_once when any socket is used on Windows, and WSACleanup() on destruction. This is a genuine second process-wide library init
 — same category as the OpenSSL one, but Windows-only and gated behind call_once.

C++ — TOutput::instance() (all platforms)

lib/cpp/src/thrift/TOutput.h:52

A process-wide logging singleton (writes to stderr by default). The function pointer it holds is replaceable via setOutputFunction(), making it a globally mutable hook into
the process's error output. Less severe than OpenSSL init, but it is shared state.

C++ — SIGPIPE (not installed — deliberately avoided)

TSocket uses MSG_NOSIGNAL on Linux and SO_NOSIGPIPE on BSD/macOS — per-socket/per-call options. Thrift does not touch the process-wide SIGPIPE handler, which is the correct
design.

D binding — SIGPIPE (documented but not installed by Thrift)

lib/d/src/thrift/server/transport/ssl.d:33–39

The D SSL transport's doc comment explicitly tells the user to call signal(SIGPIPE, SIG_IGN) themselves before using OpenSSL over the binding. Thrift-D does not install it,
but it is documented as the caller's responsibility — a soft process-wide side effect that's delegated outward.

Go — init() functions (benign)

Three init() calls in numeric.go, compact_protocol.go, simple_json_protocol.go. All initialize package-level data (constant sentinels, a type-map, byte-slice literals). No
I/O, no library hooks.

All other bindings

Java, Python, Ruby, Perl, Rust, PHP, JavaScript/Node.js, netstd, Haxe, Kotlin: no process-wide initialization side effects found. SSL in these bindings delegates
entirely to the runtime's standard TLS stack (javax.net.ssl, Python's ssl module, IO::Socket::SSL, etc.), which do not require explicit library init calls.

---
Summary: the only genuine process-wide initializations are C++ OpenSSL (already known), C++ WSAStartup on Windows, and the C++ TOutput singleton. Everything else is either
per-object, per-call, or delegated to the language runtime.

**Q29.** 
The picture is nuanced by binding:

C++ (lib/cpp) writes to stderr by default in normal (non-debug) operation:
- TOutput::errorTimeWrapper (the default output sink) calls fprintf(stderr, ...) — TOutput.cpp:98. This is configurable via setOutputFunction or silenced with
-DTHRIFT_SQUELCH_CONSOLE_OUTPUT.
- T_ERROR / T_LOG_OPER macros in TLogging.h:107,150 write directly to stderr unconditionally (not routed through TOutput).
- T_DEBUG* macros are compiled out at T_GLOBAL_DEBUGGING_LEVEL = 0 (the default), so those are silent.

Other bindings — closer to your description:
- Java: Uses SLF4J (a proper logger facade); the System.out.println lines in TMultiplexedProtocol.java are in Javadoc comments, not executed code.
- Go: Had a Logger function-type facade (logger.go:41) but it's now deprecated and no longer used by library code — current Go library propagates errors without writing to
stderr.
- netstd / Python: No direct console writes found in library source.

Summary: The claim holds for Java, netstd, and Python. For C++, it's partially true — debug output is off by default, but error/operational messages do land on stderr via
TOutput (replaceable) and some macros that bypass it entirely. Go has deprecated its logger facade and now just returns errors.

**Q30.** 
Thrift opens only those files the application explicitly hands to `TFileTransport` / per-binding equivalents.

**Q31.**
Thrift does not touch the locale or FPU state.

---

### Wave 3 — per-binding parity (where C++/Java/Python/Go/Rust are reference)

The model assumes the canonical bindings define the contract and others follow. Identify gaps.
=> Not always. The uuid type was first implemented in C# netstd, for example.

**Q40.** 
`TConfiguration` with `maxMessageSize` exists in C++, Java, Go, Rust and a few more (after some release-N), and the default is finite there. 

Bindings with TConfiguration / maxMessageSize

All eight have the same default: 100 MB (100 × 1024 × 1024 bytes):

```
┌─────────────┬─────────────────────────────────────────┬─────────┐
│   Binding   │                  File                   │ Default │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ C++         │ lib/cpp/src/thrift/TConfiguration.h     │ 100 MiB │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ Java        │ lib/java/…/TConfiguration.java          │ 100 MiB │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ Go          │ lib/go/thrift/configuration.go          │ 100 MiB │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ Rust        │ lib/rs/src/configuration.rs             │ 100 MiB │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ netstd (C#) │ lib/netstd/Thrift/TConfiguration.cs     │ 100 MiB │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ Haxe        │ lib/haxe/src/…/TConfiguration.hx        │ 100 MiB │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ c_glib      │ lib/c_glib/…/thrift_configuration.h     │ 100 MiB │
├─────────────┼─────────────────────────────────────────┼─────────┤
│ Delphi      │ lib/delphi/src/Thrift.Configuration.pas │ 100 MiB │
└─────────────┴─────────────────────────────────────────┴─────────┘
```

Rust caveat: TConfigurationBuilder::new() initialises max_message_size to None (unlimited); only TConfiguration::default() / TConfigurationBuilder::default() applies the 100
MiB limit. The other languages always default to 100 MiB regardless of how the object is constructed.

Bindings without TConfiguration / maxMessageSize (no enforced limit)

Python, Ruby, PHP, JavaScript (lib/js), Node.js, TypeScript (nodets/ts), Perl, Erlang, D, Lua, Common Lisp, Kotlin, Smalltalk, OCaml, Dart, Java ME.


**Q39.** 
The claim needs significant corrections. Here's what the code actually shows:

---
TFramedTransport — frame size limit

In C++, Java, Go, and Rust the limit lives on TConfiguration, not on TFramedTransport itself. There is no setMaxFrameSize method on TFramedTransport in any of those four — the
 transport reads its limit from the configuration object passed at construction. All four share the same constant:

```
┌─────────┬──────────────────────────────────────────────────────────┬────────────┐
│ Binding │                         Constant                         │   Value    │
├─────────┼──────────────────────────────────────────────────────────┼────────────┤
│ C++     │ TConfiguration::DEFAULT_MAX_FRAME_SIZE                   │ 16,384,000 │
├─────────┼──────────────────────────────────────────────────────────┼────────────┤
│ Java    │ TConfiguration.DEFAULT_MAX_FRAME_SIZE                    │ 16,384,000 │
├─────────┼──────────────────────────────────────────────────────────┼────────────┤
│ Go      │ DEFAULT_MAX_FRAME_SIZE (+ deprecated DEFAULT_MAX_LENGTH) │ 16,384,000 │
├─────────┼──────────────────────────────────────────────────────────┼────────────┤
│ Rust    │ TConfiguration::DEFAULT_MAX_FRAME_SIZE                   │ 16,384,000 │
└─────────┴──────────────────────────────────────────────────────────┴────────────┘
```

Rust caveat (same as maxMessageSize): TConfigurationBuilder::new() initialises max_frame_size to None (unlimited); only TConfigurationBuilder::default() applies the 16,384,000
 limit.

Python TFramedTransport: no frame-size enforcement exists at all — no constant, no setter, no check.

---
THeaderTransport — frame size limit

The picture is different and less uniform:

```
┌─────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┬─────────────────────────────┐
│ Binding │                                                        Where                                                        │     Default / hard cap      │
├─────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────────────┤
│ C++     │ THeaderTransport::MAX_FRAME_SIZE (static const, no setter)                                                          │ 0x3FFFFFFF (~1 GiB)         │
├─────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────────────┤
│ Java    │ reads TConfiguration.getMaxFrameSize()                                                                              │ 16,384,000                  │
├─────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────────────┤
│ Go      │ both THeaderMaxFrameSize = 0x3fffffff (hard cap) AND cfg.GetMaxFrameSize() (16,384,000) — whichever is smaller wins │ effective limit: 16,384,000 │
├─────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────────────┤
│ Python  │ set_max_frame_size() exists; default is HARD_MAX_FRAME_SIZE = 0x3FFFFFFF                                            │ ~1 GiB                      │
├─────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────────────┤
│ Rust    │ no THeaderTransport in lib/rs                                                                                       │ —                           │
└─────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴─────────────────────────────┘
```

---
Summary of what's wrong with the original claim:
- The setter is on TConfiguration, not on TFramedTransport, in all compiled-language bindings.
- Python TFramedTransport has no frame size limit.
- Python THeaderTransport defaults to 0x3FFFFFFF, not 16 MiB.
- C++ THeaderTransport has a hard-coded 0x3FFFFFFF cap with no setter.
- Rust has no THeaderTransport.

**Q47.** 
`TFramedTransport` buffers a whole frame before deserializing; `TBufferedTransport` buffers reads/writes; `TSocket` is unbuffered. Frame-size cap, if set, bounds buffered consumption. Note that `TFramedTransport` and  `TBufferedTransport`  are what Thrift calls "layered transport" wrappers, while `TSocket` is an endpoint transport. 

**Q48.** 
Thrift does not support memory-mapped transports as a first-class mode yet. This may be added in the future.

**Q49.** 
Thrift does no rate-limiting at any layer; rate-limiting is the operator's / load-balancer's job.

**Q50.** 
A binding that exposes `setContainerSizeLimit` but does not enforce it when called is a `VALID` bug. List bindings (if any) where caps are partially or completely ignored.

**Q44.** 
In all bindings, the default Binary message-encoding mode is "accept both strict and old encoding on read; emit strict on write"

**Q19.** 
Your framing needs one correction: it is not a build flag.

What it actually is: a C++ template parameter on TBinaryProtocolT<Transport_, ByteOrder_>. The default is TNetworkBigEndian; opting in requires instantiating with
TNetworkLittleEndian explicitly, either directly or via the pre-made typedefs:

// lib/cpp/src/thrift/protocol/TBinaryProtocol.h:206,265
typedef TBinaryProtocolT<TTransport, TNetworkLittleEndian>        TLEBinaryProtocol;
typedef TBinaryProtocolFactoryT<TTransport, TNetworkLittleEndian> TLEBinaryProtocolFactory;

There is no cmake/build-system flag that switches the default — the standard TBinaryProtocol typedef is always big-endian. Little-endian is a per-instantiation,
source-code-level choice, not a compilation option.

Is the feature still present? Yes. Both typedefs above are in the current header, and TNetworkLittleEndian is exercised in lib/cpp/test/Benchmark.cpp (lines 107, 121, 198,
210). The comment in TProtocol.h:666 explains the motivation: "On most systems, this will be a bit faster than TNetworkBigEndian" — it's a performance optimisation, not an
alternate protocol.

Your other two points are correct:
- Not interoperable with any non-C++ peer (no other binding has a little-endian binary mode).
- Entirely operator opt-in — it requires a deliberate source-level decision to use TLEBinaryProtocol instead of TBinaryProtocol.


**Q37.** 
Two corrections for the table:

Flag name — there is no named flag. The spec doc (doc/specs/thrift-binary-protocol.md:53–54) describes it only as "the option to use the binary protocol with little endian
order" without giving it an identifier. The mechanism is a C++ template parameter: ByteOrder_ on TBinaryProtocolT<Transport_, ByteOrder_>. The opt-in types are
TNetworkLittleEndian (passed explicitly) or the pre-made convenience aliases TLEBinaryProtocol / TLEBinaryProtocolFactory. There is no cmake option, preprocessor macro, or
named flag.

Default — confirmed as TNetworkBigEndian (the template default at TBinaryProtocol.h:37), i.e. standard big-endian / off.

So the table entry should read something like:

```
┌──────────────────────────────────────────────────────────────────────────┬────────┬─────────────────────────┬─────┐
│                               Knob / flag                                │ Family │         Default         │  …  │
├──────────────────────────────────────────────────────────────────────────┼────────┼─────────────────────────┼─────┤
│ ByteOrder_ template parameter (TNetworkLittleEndian / TLEBinaryProtocol) │ C++    │ TNetworkBigEndian (off) │ …   │
└──────────────────────────────────────────────────────────────────────────┴────────┴─────────────────────────┴─────┘

The "default: off" in your table is correct; "flag name" needs to be replaced with the template parameter / typedef names above, since no single named flag exists.
```

**Q45.** 
Here is the verified inventory, security-relevant options first:

---
Security-relevant generator options

Java

```
┌─────────────────┬─────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│     Option      │ Default │                                                                    Effect                                                                     │
├─────────────────┼─────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ unsafe_binaries │ off     │ Skips array.clone() when wrapping byte[] into ByteBuffer. Caller receives a reference into the transport's raw backing array. If the buffer   │
│                 │         │ is pooled or reused, a later read mutates previously returned data — information disclosure / data corruption.                                │
├─────────────────┼─────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ reuse_objects   │ off     │ Reuses deserialized struct/container instances across reads instead of allocating fresh ones. Fields absent in a later message retain their   │
│                 │         │ values from the previous one — state bleed between messages.                                                                                  │
└─────────────────┴─────────┴───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

Python

```
┌────────────────┬────────────┬────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│     Option     │  Default   │                                                                   Effect                                                                   │
├────────────────┼────────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ no_utf8strings │ off (UTF-8 │ Disables automatic UTF-8 encode/decode wrapping on string fields. On Python 2 paths this bypasses the UTF-8 validation the default mode    │
│                │  on)       │ enforces. (utf8strings is also accepted but deprecated and a no-op since it is now the default.)                                           │
└────────────────┴────────────┴────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

netstd (C#)

```
┌─────────────┬─────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│   Option    │ Default │                                                                      Effect                                                                       │
├─────────────┼─────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ no_deepcopy │ off     │ Suppresses generation of DeepCopy() methods on structs. Not wire-affecting, but callers that share or cache deserialized objects lose the only    │
│             │         │ in-library mechanism for safe copying — accidental aliasing risk.                                                                                 │
├─────────────┼─────────┼───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ serial /    │ off     │ Emits [DataContract] / [DataMember] XML serialization attributes. Opens a second deserialization pathway (WCF / XML serializer) on the same       │
│ wcf         │         │ generated types, widening attack surface.                                                                                                         │
└─────────────┴─────────┴───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

---
Wire-format / interoperability-affecting options (not strictly security, but affect what peers can connect)

```
┌──────────┬───────────────────────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Language │          Option           │                                                               Effect                                                                │
├──────────┼───────────────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ Erlang   │ string=binary|string|both │ Changes the Erlang type produced/consumed for string fields. binary and string reject input of the other form at runtime. Default:  │
│          │                           │ both. Peers using different values are interoperable on the wire but may reject valid messages at the application layer.            │
├──────────┼───────────────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ Erlang   │ set=v1|v2                 │ Switches set container implementation (ordsets vs sets module). Wire bytes are identical; only in-process representation differs.   │
│          │                           │ Default: v1.                                                                                                                        │
├──────────┼───────────────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ Java     │ sorted_containers         │ Uses TreeMap/TreeSet instead of HashMap/HashSet. Wire bytes identical; ordering is deterministic.                                   │
├──────────┼───────────────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ Java     │ option_type=jdk8|thrift   │ Changes the Optional<> wrapper type for optional fields. Wire serialization is unchanged; affects API surface only.                 │
└──────────┴───────────────────────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

---
Confirmed absent

Go (t_rs_generator.cc has no option parsing at all beyond the language name), Haxe (callbacks, rtti, buildmacro — none wire/security affecting), Delphi (register_types,
old_names, constprefix, events, xmldoc, async, com_types, rtti, guid_v4 — none wire-affecting beyond COM type registration).

---
Bottom line: The two genuine security risks in current code are Java unsafe_binaries (raw buffer exposure) and Java reuse_objects (message state bleed).


---

### Wave 4 — Boundary semantics and reachability

**Q13.** 
The wire trust boundary sits at the socket, with the bytes remaining untrusted through the generated `read()` calls and becoming trusted only when the handler chooses to act on them.

**Q14.** 
The client-side wire trust boundary is symmetric to the server-side one.

**Q15.** The in-process serializer (e.g., `TMemoryBuffer` + `TBinaryProtocol` reading a persisted file) has the same trust posture as the network case — untrusted bytes, trusted caller — but per-binding `maxMessageSize` enforcement may differ.

**Q16.** 
An IDL-compiler finding is in-model only if reachable from a `.thrift` file consumed by the **build pipeline** from a trusted source.

**Q17.** Serializer-direction (`T*Protocol::write*`) bugs are in-model only when the input struct fields can be attacker-influenced (e.g., server echoing user input back). Pure writer-side bugs fed by trusted in-process data are `OUT-OF-MODEL: trusted-input`.

**Q46.** **Spec ambiguity (D10).** 
Reject malformed type-tag with a protocol exception" as a P8a property.

---

### Wave 5 — properties: what holds, under what conditions

**Q18.** 
Thrift assumes the host OS provides working sockets, TLS-capable cryptography, and a working allocator; Thrift implements none of these itself.

**Q20.** Integer overflow on any value derived from wire-supplied lengths (sizes, frame lengths, container counts) is a security-relevant bug, full stop.

**Q21.** 
Requires Java 8 or later. When SASL transports are used, the JVM must provide javax.security.sasl, javax.net.ssl, and javax.security.auth.callback (all standard in Java SE
8+). The JCE provider chosen by the operator supplies the actual cryptographic primitives; no specific provider is required or bundled.

**Q22.** 
TLS comes from JSSE; Thrift does not bundle a TLS impl.

**Q23.** 
TLS comes from the `ssl` stdlib; Thrift Python does not bundle openssl.

**Q24.**
"Python 3.x only" is correct. The floor, based on what CI actually tests, is Python 3.10.

build.yml:680 declares the test matrix:
python-version: &python_versions ["3.10", "3.11", "3.12", "3.13", "3.14"]

Two things worth noting:

1. No python_requires is declared in setup.py — the metadata only carries 'Programming Language :: Python :: 3'. Pip will not enforce the floor at install time. If you want to
state 3.10 in the security advisory, it is an accurate description of what is tested, but the package itself doesn't reject older Pythons.

2. Dead shim for Python < 3.5 — setup.py:105 still gates backports.ssl_match_hostname on hexversion < 0x03050000. That branch is unreachable given the 3.10 CI floor and could
be cleaned up, but it doesn't affect correctness.

So the supported floor to state in documentation is Python 3.10, with the caveat that python_requires is not formally set in the package metadata.

**Q54.** 
P1 covers all of Java/Python/Go/Rust/JavaScript/Ruby/etc. — memory safety is provided by the language runtime, and Thrift does not invoke `unsafe`/`Unsafe`/`ctypes`. A stack-overflow on recursive `skip` is in-scope as a P1 violation.

**Q55.** 
P2 conditions are size caps set, OpenSSL contract observed, no integer overflow on declared sizes; CVE-worthy violations are heap corruption / use-after-free / OOB write reachable from the wire.

**Q56.** **(Per-connection thread safety.)** 
Each connection runs on its own thread/coroutine — mostly true. Exceptions: TSimpleServer in C++/Java/Python is single-threaded and serializes connections; TThreadPoolServer
in all three languages is pool-bounded, not one-thread-per-connection. Go's TSimpleServer uses goroutines (cheap, but still concurrent).

Thrift runtime state not shared — true.

Handler synchronization is the application's job — true, but only when the same processor/handler object is reused across connections, which is the default for the common
single-TProcessor constructor path.


**Q57.** **(TLS confidentiality/integrity.)** 
P7 is essentially "we wrap the TLS library faithfully; the cryptographic guarantee is theirs". 
Past instances where Thrift broke a TLS library's contract:

Here is a full inventory from the git history, grouped by failure type:

---
1. Protocol version toggle — defaulting into broken versions

THRIFT-3164 (C++, 2015, b0b710a)
The C++ SSLContext did not set SSL_OP_NO_SSLv3 by default. All server and client connections were willing to negotiate SSLv3 (POODLE), with no mechanism in the constructor to
refuse it. Fixed by adding the option unless the caller explicitly opts back in.

THRIFT-4084 (csharp, D, Go, Node.js, Perl, 2017, 06190874)
The cross-feature test revealed that csharp, D, Go, Node.js, and Perl were also still negotiating SSLv3. Each required its own SSL_OP_NO_SSLv3 / equivalent toggle. This was the
 system-wide survey that found THRIFT-3164 was not an isolated case.

---
2. Hostname verification not wired up

THRIFT-3009 (Go, 2015, 00a4e3e8)
TSSLSocket in Go was passing only the resolved net.Addr to tls.Dial, discarding the hostname string. The Go TLS stack needs the hostname to match the server certificate CN/SAN;
 without it the certificate was accepted for any hostname.

THRIFT-5343 (netstd, 2021, 55016bff)
TTlsSocketTransport.OpenAsync() resolved the hostname to an IP address and then passed _host.ToString() (the IP) as the targetHost argument to AuthenticateAsClient(). This made
 .NET's SslStream validate the certificate against "192.168.x.y" instead of the original DNS name, causing silent hostname mismatch on any certificate without a Subject
Alternative Name for the IP.

Java TSSLTransportFactory + TNonblockingSSLSocket (Java, multiple attempts 2026, 30c567dc→a30c552b, 68ac8e93)
Neither TSSLTransportFactory.createClient() nor TNonblockingSSLSocket called setEndpointIdentificationAlgorithm("HTTPS") on the SSLParameters. Java's JSSE does not perform
hostname verification unless this flag is set; the TLS handshake could succeed against a certificate whose CN/SAN did not match the target host. The two client paths also
diverged (blocking vs. async), requiring separate fixes.

---
3. Sending data before the TLS handshake completes

THRIFT-3786 (Node.js, 2016, 0ea6c1d5)
The Node.js Connection constructor listened on the 'connect' event instead of 'secureConnect'. TCP 'connect' fires when the TCP layer is up; TLS is not yet established. Thrift
was marking the connection ready and draining its offline write queue — sending plaintext frames — before the TLS handshake had completed. Fixed by switching the listener to
this.ssl ? 'secureConnect' : 'connect'.

---
4. SSL I/O retry loop violating OpenSSL's want-read/want-write contract

THRIFT-4331 (C++, 2017, 9f9e30b5)
TSSLSocket::read() and write() called waitForEvent(wantRead) where the argument drove whether to POLLIN or POLLOUT. During a TLS renegotiation, OpenSSL can return
SSL_ERROR_WANT_READ on a write call and SSL_ERROR_WANT_WRITE on a read call. The socket was polling on the wrong direction, causing the retry to either deadlock or time out on
large messages. Fixed by always including POLLIN in the write-wait bitmask and decrementing the retry counter correctly only when OpenSSL signals no data is available yet.

---
5. SSL buffer contract — readpartial vs. OpenSSL's internal buffer

Ruby socket (Ruby, 2026, 8a358b8c)
TSocket.read() used readpartial, which reads from the OS socket buffer. Ruby's OpenSSL bindings maintain a separate internal decrypted-data buffer; once data has been received
and decrypted it is not "visible" to IO.select as readable. Calling readpartial when OpenSSL's buffer has data but the socket has nothing pending returns 0 or blocks. Fixed by
switching to read_nonblock with WaitReadable/WaitWritable rescue to properly drain the SSL layer.

---
6. OpenSSL global lifecycle contract violated

THRIFT-3944 (C++, 2016, bede86a0)
checkHandshake() created two BIO_s_mem() (memory-backed) BIOs, called BIO_set_nbio on them, then immediately called SSL_set_fd() — which installs a new socket BIO and
implicitly frees whatever was set before. The memory BIOs were created and leaked; the BIO_set_nbio calls had no effect. This was dead code performing meaningless OpenSSL
operations on every TLS handshake.

THRIFT-4164 (C++, 2017, 7f5a8c28)
cleanupOpenSSL() was not calling ENGINE_cleanup() (required before OpenSSL 1.1.0) and not calling CONF_modules_unload(1). The documented OpenSSL teardown sequence was
incomplete; this caused memory leaks and, in certain embedding scenarios, use-after-free crashes when OpenSSL was initialized more than once in a process lifetime.

THRIFT-3878 (C++, 2016, 4bbfe612)
OpenSSL 1.1 made CRYPTO_num_locks() a no-op macro. Thrift was still calling it to size a mutex array, which vanished under the new header. Fixed with an #ifdef CRYPTO_num_locks
 guard.

---
7. SSLSocket error semantics misread

THRIFT-4559 (C++, 2018, b33130f6)
On SSL_ERROR_SYSCALL with errno == 0 and ERR_peek_error() == 0, the OpenSSL manual specifies that the peer sent a close_notify. Thrift was trying to call ERR_error_string on a
zero error code, printing garbage. The underlying contract (poll the error queue before calling ERR_error_string) was not followed.

THRIFT-5595 (Python, 2022, e3eb9afb)
Python's ssl.SSLSocket.recv() rejects calls with non-zero flags. A preceding refactor in TSocket added recv(1, socket.MSG_PEEK) to implement isOpen(). This raises ValueError on
 SSLSocket, not the expected closed-socket indication.

---
8. Duplicating TLS library hostname matching in user space

Python sslcompat (Python, 2024, 23e0e5ce)
Thrift had its own legacy_validate_callback using ssl.match_hostname() for post-handshake hostname checking. Python's ssl module has delegated hostname matching to OpenSSL
since 3.7; ssl.match_hostname() was removed in 3.12. The fix: bypass the redundant callback on Python ≥ 3.12 since OpenSSL already validated the hostname. The root issue was
Thrift re-implementing a check the TLS library already performs.

---
Pattern summary: The violations fall into four recurring categories — (a) protocol-version defaults set too permissively, (b) hostname verification not delegated to the TLS
stack, (c) handshake lifecycle events misidentified (data sent too early or I/O retried on wrong poll direction), and (d) OpenSSL global state init/teardown not following the
library's documented sequence.



**Q58.** 
Answer for each binding:

---
C++ — Not automatic; requires two explicit calls

The TSSLSocketFactory constructor sets neither SSL_VERIFY_PEER nor any trust store. The default OpenSSL context mode is SSL_VERIFY_NONE.

Post-handshake, authorize() always calls SSL_get_verify_result() and throws on any non-X509_V_OK result (TSSLSocket.cpp:714–716). From the OpenSSL man page: even under
SSL_VERIFY_NONE, the client-side certificate IS verified internally; the flag only controls whether the handshake aborts on failure. So authorize() does provide enforcement.

But no CA bundle is loaded. TSSLSocketFactory never calls SSL_CTX_set_default_verify_paths(). With no trust anchors, OpenSSL returns
X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY for every real certificate, and authorize() throws — including for valid certificates from well-known CAs.

The user must make two explicit calls for correct operation:
factory.loadTrustedCertificates("/path/to/ca-bundle.pem");
factory.authenticate(true);  // sets SSL_VERIFY_PEER

Without both, connections either throw for all certs (no CAs loaded) or operate with the wrong OpenSSL mode. The claim "automatically against the configured trust store" is
false for C++: nothing is configured and nothing is automatic.

---
Java — Automatic on the default path; explicit on the param path

getClientSocket(host, port) uses SSLSocketFactory.getDefault(), which delegates to JSSE. JSSE automatically uses the JVM's default trust store
($JAVA_HOME/lib/security/cacerts). Chain validation is performed by the JSSE implementation, not by Thrift code. createClient() at TSSLTransportFactory.java:342–356 also now
sets EndpointIdentificationAlgorithm = "HTTPS", so hostname verification is included.

The getClientSocket(host, port, timeout, params) path requires the caller to supply TSSLTransportParameters with at least one of isKeyStoreSet or isTrustStoreSet set — the
factory throws (TSSLTransportFactory.java:109) if neither is set.

The claim is correct for the default no-params path. For the custom-params path, the trust store is explicit but required.

---
Python — Validation is enabled by default but ca_certs is mandatory

Default cert_reqs = ssl.CERT_REQUIRED (TSSLSocket.py:146), so chain validation is requested. However, if ca_certs is None (the default) and cert_reqs != ssl.CERT_NONE, the
constructor raises ValueError at line 158:

if self._should_verify:
    if not self.ca_certs:
        raise ValueError('ca_certs is needed when cert_reqs is not ssl.CERT_NONE')

The system CA bundle is not loaded automatically. _wrap_socket() only calls ssl_context.load_verify_locations(self.ca_certs) when ca_certs is set (line 186–187). The user must
provide the CA file path explicitly.

The claim is partially correct: validation is on by default, but a ca_certs argument is mandatory for the socket to even construct. No automatic system-store fallback.

---
Go — Correct; fully delegates to crypto/tls defaults

TSSLSocket.Open() calls tls.DialWithDialer() with whatever *tls.Config the caller supplies (ssl_socket.go:166–199). GetTLSConfig() returns nil if no config is set
(configuration.go:201–205).

Go's crypto/tls with a nil config (or a zero-value tls.Config) has InsecureSkipVerify = false by default, so cert chain validation IS performed. When RootCAs is nil, Go uses
the system root CA pool automatically via x509.SystemCertPool().

The claim is correct for Go: validation is on by default, and the system store is used if no explicit pool is provided. The only way to bypass it is for the caller to set
InsecureSkipVerify: true in their *tls.Config, which is outside Thrift's control.

---
Rust — No TLS support

The Rust library (lib/rs/src/transport/socket.rs) implements only plain TCP via TcpStream. There is no TSSLSocket type, no TLS transport, and no SSL dependency. The claim is
inapplicable.

---
Summary table

```
┌─────────┬─────────────────────────────────────────┬───────────────────────────────────────────┬────────────────────┬─────────────────────────────────────────────────────┐
│ Binding │        Chain validation default         │            Trust store source             │   System store     │                       Caveat                        │
│         │                                         │                                           │    auto-loaded?    │                                                     │
├─────────┼─────────────────────────────────────────┼───────────────────────────────────────────┼────────────────────┼─────────────────────────────────────────────────────┤
│ C++     │ Enforced via authorize() post-handshake │ Must call loadTrustedCertificates()       │ No                 │ Factory sets no verify mode and no CAs; without     │
│         │                                         │                                           │                    │ explicit setup all connections fail                 │
├─────────┼─────────────────────────────────────────┼───────────────────────────────────────────┼────────────────────┼─────────────────────────────────────────────────────┤
│ Java    │ JSSE enforces automatically             │ JVM cacerts (default) or explicit         │ Yes (JVM default)  │ Custom-params path requires explicit truststore     │
│         │                                         │ TSSLTransportParameters                   │                    │                                                     │
├─────────┼─────────────────────────────────────────┼───────────────────────────────────────────┼────────────────────┼─────────────────────────────────────────────────────┤
│ Python  │ CERT_REQUIRED by default                │ Must provide ca_certs= path               │ No                 │ Constructor raises ValueError if ca_certs is None   │
│         │                                         │                                           │                    │ and validation requested                            │
├─────────┼─────────────────────────────────────────┼───────────────────────────────────────────┼────────────────────┼─────────────────────────────────────────────────────┤
│ Go      │ crypto/tls enforces (default            │ System root CA pool if RootCAs=nil        │ Yes (system pool)  │ Caller can bypass by setting InsecureSkipVerify:    │
│         │ InsecureSkipVerify=false)               │                                           │                    │ true                                                │
├─────────┼─────────────────────────────────────────┼───────────────────────────────────────────┼────────────────────┼─────────────────────────────────────────────────────┤
│ Rust    │ N/A                                     │ N/A                                       │ N/A                │ No TLS transport exists                             │
└─────────┴─────────────────────────────────────────┴───────────────────────────────────────────┴────────────────────┴─────────────────────────────────────────────────────┘
```

**Q59.** 
How the cap/allocation machinery actually works

The check (all bindings, same pattern):

readListBegin / readMapBegin calls checkReadBytesAvailable(declaredCount * getMinSerializedSize(elemType)). The minimum sizes are:

┌────────────────┬───────────────────┬────────────┐
│      Type      │      Binary       │  Compact   │
├────────────────┼───────────────────┼────────────┤
│ T_BOOL/BYTE    │ 1                 │ 1          │
├────────────────┼───────────────────┼────────────┤
│ T_I32          │ 4                 │ 1 (zigzag) │
├────────────────┼───────────────────┼────────────┤
│ T_STRING       │ 4 (length prefix) │ 1 (varint) │
├────────────────┼───────────────────┼────────────┤
│ T_STRUCT       │ 1 (T_STOP only)   │ 1          │
├────────────────┼───────────────────┼────────────┤
│ T_LIST/MAP/SET │ 4 (count header)  │ 1          │
└────────────────┴───────────────────┴────────────┘

The pre-allocation (immediately follows the check):
- C++: prefix.resize(declaredCount) → allocates declaredCount × sizeof(Foo) before reading any element
- Java: new ArrayList<>(declaredCount) → same pattern (NestedListsI32x3.java:350)
- Go: make([]T, 0, declaredCount) → allocates backing array of that capacity

C++ cumulative vs. Go per-field:

C++/Java use a decreasing remainingMessageSize_ counter — each wire byte consumed reduces the budget. Go's checkSizeForProtocol compares against the full maxMessageSize each
time (configuration.go:319–333), not a running remainder. A Go struct with k list fields can each declare maxMessageSize / 1 elements independently.

---
Evaluation of the three clauses

Clause 1: "Super-linear memory or CPU in the declared size of any single field, given caps are set, is a bug"

Memory: For any single field, memory is O(N) where N = declared count. Lists pre-allocate exactly N slots; maps and sets grow element-by-element. No field in isolation causes
super-linear memory. ✓ Clause matches reality.

CPU: For C++ std::map / std::set: tree insertions are O(N log N) in the declared count — technically super-linear. Java/Python/Go use hash containers: O(N) average. The policy
would classify C++ ordered-container insertion as a bug by this definition, which is probably not the intent — it's inherent to the data structure, not an amplification
introduced by Thrift.

Verdict on clause 1: Correct for memory. For CPU, it over-includes ordered-container inherent complexity (std::map/std::set). You need a carve-out: "super-linear excluding O(N
log N) for ordered container types whose element ordering requires comparison-based insertion."

---
Clause 2: "Constant-factor blowup against caps is not [a bug]"

This is where the actual behaviour lives, and the policy is structurally correct but the constant can be extremely large.

For list<LargeStruct> with maxMessageSize = M:
- checkReadBytesAvailable(N × 1) passes when N ≤ M (T_STRUCT min-size = 1 in binary)
- resize(N) allocates N × sizeof(LargeStruct) bytes
- Amplification factor = sizeof(LargeStruct) / 1 = sizeof(LargeStruct)

If sizeof(LargeStruct) = 200 bytes: a 100 MB cap permits a 20 GB allocation from a single list field. This is "constant-factor" in the policy's sense (the constant is fixed per
 schema), but it can be 100–10,000× depending on the struct.

An additional complicating factor: C++ binary protocol has a separate container_limit_ (per-collection element count cap, TBinaryProtocol.h:48,66) that defaults to 0
(disabled). The checkReadBytesAvailable path and the container_limit_ path are independent guards. The message-size cap alone is not sufficient to bound memory if struct
element types have large in-memory footprint; container_limit_ is needed too. The policy doesn't mention this second lever.

Verdict on clause 2: The framing is correct. The qualification needed: "the constant factor is sizeof(in-memory-type) / getMinSerializedSize(wire-type) and is schema-dependent
— for struct collections under binary protocol this can exceed 100×. container_limit_ (C++) provides an independent bound on N directly when the message-size cap alone is
insufficient."

---
Clause 3: "A hang on streaming input with no cap set is the operator's choice, not a bug"

Framed transports (TFramedTransport): the 4-byte frame-size header is an implicit cap, separate from maxMessageSize. A hang is structurally impossible — the frame size is read
first and bounds the read.

Unframed transports (plain TSocket / TBufferedTransport): with maxMessageSize unconfigured or set to 0, the server loops reading until the client closes or the allocation fails
 with OOM. The policy's statement is accurate here.

One gap: setting maxMessageSize to 0 in TConfiguration is not the same as "no cap set" — it may mean "unlimited" depending on the binding. The Java TConfiguration treats 0 as
unlimited; C++ initialises to 100 MB by default. The operator must actively increase or zero the cap to get the unlimited-read behaviour.

Verdict on clause 3: Accurate for unframed transports. Add: "framed transports cannot hang on this path regardless of cap configuration."

---
What the policy doesn't cover

1. Go's non-cumulative check. In Go, each collection's declared size is checked against the full maxMessageSize, not against remaining budget. A struct with k fields each
containing a list<struct> can declare up to k × maxMessageSize / 1 elements total, causing k × maxMessageSize × sizeof(Struct) memory. This is super-linear in k (number of
fields), but not in any single field's declared size — so the policy wouldn't classify it as a bug even though the aggregate is unbounded relative to the cap.
2. The recursion-depth cap is a third independent lever. recursionLimit (TConfiguration) is orthogonal to both maxMessageSize and container_limit_. Deeply nested schemas need
all three set to bound worst-case resource use.
3. Cross-field amplification is outside the policy scope. The policy only covers "any single field." Multiple fields together can exceed bounds in ways the policy silently
permits.

---
Bottom line

The policy is acceptable as a categorical line with three required qualifications:

1. Carve out O(N log N) for ordered containers (std::map/std::set CPU): this is inherent data structure complexity, not a Thrift amplification bug.
2. Note that "constant-factor" is schema-dependent and can be 100–10,000× for struct-element collections under binary protocol; container_limit_ (C++) is the additional lever,
not just maxMessageSize.
3. Note framed transports cannot hang on this path regardless of cap setting, and that Go's cap check is per-field (not cumulative), which the policy's "single field" framing
happens to fit but for different structural reasons than C++/Java.


**Q60.** **(Codegen determinism.)** 
The IDL compiler given identical `.thrift` files + flags emits byte-identical generated code across runs and platforms. Generated-code stability is part of the contract. Only exception is the usage of V4 GUIDs for generated Delphi interfaces, which has been covered by using v8 GUIDs now.

---

### Wave 6 — non-properties and false friends

**Q61.** 
Thrift provides no application-layer authorization; once a peer is past the transport authentication (TLS/SASL/none), they can invoke any service method registered with the processor.

**Q62.** 
Exposing a `T*Server` on a public socket without setting size caps" is `OUT-OF-MODEL: non-default-build`

**Q63.** 
If `TZlibTransport` has no built-in decompressed-size cap, this qualifies as a bug that should be fixed.

**Q64.** 
`TJSONProtocol` has no built-in JSON-nesting depth cap; unexpected/extra JSON nesting triggers unbounded recursion in skip() for unhardened bindings.

**Q65.**
No constant-time comparison primitives exist anywhere in Thrift; do not use Thrift-deserialized data as the substrate for HMAC verification.

**Q66.** 
`seqid` is a response-matching sequence number, not a nonce, not a session token, not a replay guard.

**Q67.** *
Thrift does not defend against socket-level DoS (slowloris, connection flood, half-open) at any layer; this is the operator's / load balancer's job.

**Q68.** 
CPU exhaustion via inputs that are individually within all configured caps (message size, container depth, recursion limit) is classified VALID-HARDENING: not a
CVE-eligible vulnerability, but a legitimate target for future optimization. Report via the public tracker. Contributions welcome.

**Q69.** 
SASL negotiation state and TLS handshake state are per-connection. Thrift maintains no cross-connection authentication context. TLS session resumption (session
tickets, session-ID caches) is delegated entirely to the underlying TLS library and is outside Thrift's threat model.

**Q70.**
TMultiplexedProcessor routes calls based on a client-supplied service-name prefix. It provides no authorization isolation between registered services: any peer
that can open a connection may address any service on that processor. Ensuring consistent authorization across all co-registered services is the application's
responsibility. Discrepancies in per-service authorization profiles are application-layer misconfigurations, not Thrift vulnerabilities.

**Q71.** 
TMultiplexedProcessor — SASL/PLAIN transmits credentials with no encryption. Thrift does not refuse this mechanism over an unencrypted TSocket; enforcing
transport security is the application's responsibility. Deployers using SASL/PLAIN must wrap the transport in TSSLSocket or an equivalent TLS layer. A finding
that SASL/PLAIN credentials are visible in cleartext over a plaintext transport is a deployment misconfiguration, not a Thrift vulnerability.

**Q72.** 
Thrift uses each host language's native map type without defensive wrapping. Hash-flooding resistance is inherited from the binding's runtime (hash
randomisation, tree-bin fallback, or ordered-map structure). A finding that a specific runtime's map is susceptible to hash-flooding is a runtime-level concern,
not a Thrift vulnerability. The C++ binding uses std::map (ordered) and is structurally immune. All other bindings rely on their runtime's existing defences.


---

### Wave 7 — adversary capabilities, TLS-transport specifics, and meta

**Q41.** 
SASL mechanism selection is the application's responsibility. Thrift does not refuse PLAIN or DIGEST-MD5. DIGEST-MD5 is deprecated per RFC 6331 and its use is
not recommended. THRIFT-5926 (crash on None initial DIGEST-MD5 response) is a correctness bug in the mechanism's handling; the fix is robust None-handling, not
mechanism removal. If the crash is reachable pre-authentication, it qualifies as a remotely-triggerable DoS and will be treated accordingly.

**Q42.** 
Thrift sets no project-wide TLS version floor. Each binding delegates version negotiation to its underlying TLS library (OpenSSL, JSSE, crypto/tls, etc.).
THRIFT-5743/5876 added TLS 1.3 capability where it was absent. Operators are responsible for configuring a version floor in the underlying library; TLS 1.2
minimum is recommended. A finding that TLS 1.0 or 1.1 is negotiable is a deployment or library-configuration concern, not a Thrift vulnerability. (Note: verify
whether the C++ binding explicitly disables SSLv2/v3 via SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3; if so, state that floor explicitly for the C++ binding.)

**Q51.**
An authenticated peer (TLS mTLS, SASL-bound, `AccessManager`-approved) is **still untrusted** at the wire-bytes level; authentication narrows who, not what they can send.

**Q52.** **(Most consequential TLS-default question.)** 
C++ client sockets created via TSSLSocketFactory automatically receive DefaultClientAccessManager, which verifies the peer hostname against SANs and CN using
Thrift's own matching logic (not OpenSSL's X509_check_host()). Client sockets constructed directly without a factory receive no AccessManager and perform no
hostname verification — only the certificate chain is validated. Server sockets perform no peer hostname verification. Operators constructing TSSLSocket directly 
are responsible for registering an AccessManager. A finding that direct construction skips hostname verification is a known design consequence, not a novel
vulnerability; a finding of a bypass in DefaultClientAccessManager::matchName() is in scope.

Most Thrift bindings perform hostname verification by default: Java (HTTPS endpoint ID), Ruby (post_connection_check), netstd (SslStream + policy-error check),
Node.js (tls.connect default). C++ (factory path) and Python (client) use Thrift's own hostname-matching code (matchName() / sslcompat._match_hostname) rather
than the underlying TLS library's built-in verifier — this is a roll-your-own concern worth flagging separately. Two paths perform no hostname verification: C++
sockets constructed directly (bypassing TSSLSocketFactory) and the c_glib binding (cert-chain only). Go delegates entirely to the caller-supplied *tls.Config;
verification depends on whether the caller sets ServerName and does not set InsecureSkipVerify.

**Q53.** 
`TSaslTransport` is plaintext over the wire unless explicitly composed with `TSSLSocket`; SASL is authentication, not transport encryption (unless the chosen mechanism happens to provide its own confidentiality, which PLAIN does not).

**Q32.** 
`THRIFT_TUUID_SUPPORT_BOOST_UUID` and `THRIFT_TUUID_BOOST_CONSTRUCTOR_EXPLICIT` change ergonomics, not security; an implicit constructor is a footgun for users but not a bug.

**Q33.** 
`FORCE_BOOST_SMART_PTR` is a build-system override that does not change the security envelope.

**Q34.** 
Any runtime check that guards memory safety or authentication, and relies solely on assert being executed, is a valid defect: assert is compiled out under
-DNDEBUG, which is standard in production builds. Per the CVE-2018-1320 precedent, such checks must be unconditional. Scope of audit: assert in any path
reachable from external input, or guarding a memcpy/pointer-arithmetic operation whose operands derive from buffer state.

**Q35.** 
ServerConnectivityCheckInterval (default 5 ms) is an operational tuning parameter. Setting it to values below ~5 ms may cause excessive CPU consumption due to Go 
runtime timer behaviour; see the Go binding README. This is a misconfiguration concern, not a vulnerability. Remote parties cannot influence the value.

**Q73.** **Coexistence with `SECURITY.md`.** 
The repo currently has no `SECURITY.md`. When one is added, this threat model should become the canonical threat-model document linked from `SECURITY.md`.

**Q74.** 
In-repo as `doc/threat-model.md`, with `SECURITY.md` and the website pointing at it.

**Q75.** 
Re-versioned every release, with §12 conditions as the trigger for "substantive" updates vs "binding refresh".

**Q76.** 
Yes

**Q77.** 
No

---

## §15 Optional: machine-readable companion

A sidecar `threat-model.yaml` is **proposed but not yet generated** (see §14 Q76). When emitted, it will mirror this document's triage-relevant facts so a static-analysis pipeline, AI-assisted reviewer, or PR-bot can route findings without parsing prose.

Proposed schema (informative, not yet canonical):

```yaml
project: apache/thrift
version: <release-tag>
sections_canonical: doc/threat-model.md

component_families:
  - name: idl_compiler
    path: compiler/cpp/
    in_scope: true
    threat_class: build-time-text-processor
  - name: cpp_runtime
    path: lib/cpp/
    in_scope: true
    threat_class: memory-unsafe-runtime
  - name: c_glib_runtime
    path: lib/c_glib/
    in_scope: true
    threat_class: memory-unsafe-runtime
  - name: jvm_runtime
    path: lib/java/
    in_scope: true
    threat_class: memory-safe-runtime
  - name: python_runtime
    path: lib/py/
    in_scope: true
    threat_class: memory-safe-runtime
  - name: go_runtime
    path: lib/go/
    in_scope: true
    threat_class: memory-safe-runtime
  - name: rust_runtime
    path: lib/rs/
    in_scope: true
    threat_class: memory-safe-runtime
  - name: other_runtimes
    path: lib/{netstd,js,ts-js,nodejs,rb,php,dart,erl,lua,hs,st,dlang,kotlin,ocaml,perl,javame,delphi,haxe}/
    in_scope: true
    threat_class: per-binding-analogous
  - name: generated_code
    path: <build-output>
    in_scope: true
    threat_class: pure-deserializer-and-dispatch
  - name: wire_specs
    path: doc/specs/
    in_scope: true
    threat_class: contract
  - name: tests_tutorials_contrib
    path: [test/, tutorial/, contrib/, compiler/cpp/test/]
    in_scope: false
    disposition: OUT-OF-MODEL:unsupported-component

build_flags:
  - name: container_size_limit_default
    family: all_runtimes
    default: INT32_MAX
    security_affecting: true
    insecure_default: PENDING_Q38
  - name: string_size_limit_default
    family: all_runtimes
    default: INT32_MAX
    security_affecting: true
    insecure_default: PENDING_Q38
  - name: access_manager_registered
    family: cpp_runtime
    default: false
    security_affecting: true
    insecure_default: PENDING_Q43
  - name: server_stop_timeout
    family: go_runtime
    default: 0
    security_affecting: true
    insecure_default: PENDING_Q36
  - name: cpp_binary_little_endian
    family: cpp_runtime
    default: false
    security_affecting: true
    discouraged_when: peers_are_non_cpp

properties_provided:
  - id: P1
    name: memory_safety_safe_runtimes
    severity_tier: high
    violation_symptom: [stack-overflow, uncaught-exception, oob-slice-via-exception]
  - id: P2
    name: memory_safety_unsafe_runtimes
    severity_tier: critical
    violation_symptom: [heap-corruption, oob-write, use-after-free, double-free]
    conditions: [caps_set, no_integer_overflow, ssl_lifetime_observed]
  - id: P3
    name: round_trip_fidelity
    severity_tier: correctness
    violation_symptom: [field-mismatch, missing-fields]
  - id: P4
    name: wire_format_conformance
    severity_tier: correctness
  - id: P5
    name: unknown_field_tolerance
    severity_tier: dos-or-correctness
  - id: P6
    name: per_connection_thread_safety
    severity_tier: correctness-with-security-impact
  - id: P7
    name: tls_confidentiality_integrity
    severity_tier: critical
    conditions: [tls_enabled, underlying_lib_correctly_configured]
  - id: P8
    name: server_cert_chain_validation
    severity_tier: critical
    conditions: [tls_enabled]
  - id: P9
    name: server_hostname_verification
    severity_tier: critical
    conditions: [access_manager_registered_or_per_binding_equivalent]
  - id: P10
    name: bounded_memory_cpu_per_message
    severity_tier: high-availability
    conditions: [caps_configured]
    threshold: super-linear-in-declared-size-given-caps  # PENDING_Q59
  - id: P11
    name: deterministic_hermetic_codegen
    severity_tier: correctness

properties_disclaimed:
  - id: D1
    name: no_byte_validation_beyond_transport_auth
  - id: D2
    name: no_application_layer_authorization
  - id: D3
    name: no_default_resource_caps
  - id: D4
    name: no_decompression_bomb_defense
  - id: D5
    name: no_json_depth_cap
  - id: D6
    name: no_constant_time
  - id: D7
    name: no_anti_replay
  - id: D8
    name: no_confidentiality_without_tls
  - id: D9
    name: no_socket_level_dos_defense
  - id: D10
    name: type_tag_mismatch_undefined_per_spec
  - id: D11
    name: no_cpu_exhaustion_defense_within_caps
  - id: D12
    name: no_cross_connection_state_isolation_guarantee
  - id: D13
    name: no_multiplexed_dispatch_authorization
  - id: D14
    name: no_endianness_interop_safety_for_cpp_le_mode

false_friends:
  - id: F1
    name: tframed_transport_is_not_a_cap
  - id: F2
    name: binary_strict_mode_is_off_by_default
  - id: F3
    name: tsslsocket_no_access_manager_means_no_hostname_check
  - id: F4
    name: getpeerhost_may_be_numeric_ip
  - id: F5
    name: sasl_plain_over_cleartext_is_cleartext_creds
  - id: F6
    name: seqid_is_not_a_session_token
  - id: F7
    name: uuid_type_has_no_cryptographic_guarantee

known_non_findings: [N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11, N12, N13, N14, N15, N16, N17, N18, N19, N20]

dispositions:
  - VALID
  - VALID-HARDENING
  - OUT-OF-MODEL:trusted-input
  - OUT-OF-MODEL:adversary-not-in-scope
  - OUT-OF-MODEL:unsupported-component
  - OUT-OF-MODEL:non-default-build
  - BY-DESIGN:property-disclaimed
  - KNOWN-NON-FINDING
  - MODEL-GAP

open_questions_count: 77
draft_confidence:
  documented: 78
  maintainer: 0
  inferred: 81
status: DRAFT-PENDING-PMC-RATIFICATION
```

The prose document is canonical until the PMC ratifies. Regenerate the sidecar on every revision.

---

## Appendix A — `SECURITY.md` back-map

The repo does not currently have a `SECURITY.md`, so the back-map is **empty**. When one is added (§14 Q73), every claim it carries must back-map to a row here.

| `SECURITY.md` statement | This document section |
| --- | --- |
| *(none — file does not exist yet)* | — |

The website's per-project page (currently only the generic `apache.org/security` pointer) similarly has no project-specific claims to back-map.

## Appendix B — Source-of-truth artifacts cited

- **In-repo READMEs read while drafting:** `README.md` (top-level), `AGENTS.md`, `FUZZING.md`, `LANGUAGES.md`, `CHANGES.md`, `lib/cpp/README.md`, `lib/java/README.md`, `lib/py/README.md`, `lib/go/README.md`, `lib/rs/README.md`.
- **Wire specs read:** `doc/specs/thrift-binary-protocol.md`, `doc/specs/thrift-compact-protocol.md`.
- **Issue-tracker entries cited via CHANGES.md** (illustrative, not exhaustive): THRIFT-4506 / CVE-2018-1320 (Java SASL assert bypass); THRIFT-4084 (SSLv3 negotiation hardening); THRIFT-5469 (Go skip-map stack overflow); THRIFT-5221, THRIFT-5255 (stack overflow on read / framed transport); THRIFT-3821, THRIFT-5716 (TMemoryBuffer overflows); THRIFT-5812 (Rust server capacity overflow); THRIFT-5931 (`thrift_ssl_socket_get_ssl_error()` stack-buffer write past end); THRIFT-5926 (TSaslClientTransport DIGEST-MD5 crash); THRIFT-5343 (TTlsSocketTransport hostname validation); THRIFT-5515 (TNonblockingServer + TSSLSocket); THRIFT-4164 (TSSLSocketFactory lifetime); THRIFT-5743, THRIFT-5876 (TLS 1.3 support).
- **GitHub security advisories pulled:** `gh api repos/apache/thrift/security-advisories` returned an empty list — Thrift uses the ASF `security@` pipeline rather than GHSA.

---

*End of draft v1. Pending PMC review per §14 wave order.*
