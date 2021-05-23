# Design

Mutual root, implement mutually

Share data structures between languages whenever possible

## Natural languages used for identifiers (Internal representations, not necessarily seen by programmers)

A mixture of Classical Chinese, Modern Chinese, Japanese and other languages

Excuse:

Formal languages are analytic languages which are not necessarily pronounceable, so I want something similar, an analytic language that does not depend on pronunciation, for identifiers.

## Platforms

- Scala3: JVM / JS / Native (As Application) / WASM
- Rust: Native (As Application) / WASM / Native (bare metal)
- Verilog: Hardware

## Versioning for Libraries and Languages

License: CC-BY 3.0

Derivative of Semantic Versioning 2.0.0

Version Number Format: MINOR.PATCH or MINOR.PATCH.PATCH

1. Incompatible API changes are not allowed unless the name of the software is changed.
1. A software MUST declare a public API. This API could be declared in the code itself or exist strictly in documentation. However it is done, it SHOULD be precise and comprehensive.
1. Once a versioned package has been released, the contents of that version MUST NOT be modified. Any modifications MUST be released as a new version.
1. A normal version number MUST take the form X.Y or X.Y.Z where X, Y and Z are non-negative integers, and MUST NOT contain leading zeroes. X is the minor version, and Y/Z is the patch version. Each element MUST increase numerically. For instance: 1.9 -> 1.10 -> 1.11.
1. Major version zero (0.y) is for initial development. Anything MAY change at any time. The public API SHOULD NOT be considered stable.
1. Increasing the minor version is RECOMMENDED to be avoided whenever possible.

