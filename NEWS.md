# bitfield 1.0.0

## New features

- New `bf_analyze()` function for exploring encoding trade-offs. Reports Pareto-optimal exponent/significand configurations per bit count, with quality metrics including RMSE, max error, underflow, overflow, and changed values at a given decimal precision. Includes a formatted `print()` method with summary header, type-specific output, and ready-to-paste `bf_map()` usage hints.
- Floating-point encode/decode rewritten from string-based to arithmetic computation, improving performance and correctness.
- Expanded validation throughout `bf_map()` and `bf_flag()` with more informative error messages.

## Improvements

- `bf_map()` now uses `template` from the registry instead of requiring explicit `length`/`width` parameters.
- Vignettes consolidated into a single "Getting Started" guide covering encoding design, `bf_analyze()`, custom protocols, and community standards.
- Substantially expanded test suite covering `bf_analyze`, `bf_export`, `bf_protocol`, helpers, and edge cases across all core functions.
- Documentation updates across all exported and internal functions.

## Bug fixes

- Fixed `bf_decode()` flag processing and sign bit handling.
- Fixed `bf_export()` metadata extraction to use correct field names from registry flags.

# bitfield 0.8.0

- introduction of `bf_flag()` function, where building the flag can be found, which is moved out from `bf_map()`.
- complete overhaul of how information is stored in the registry, now it is vastly more W3C PROV compliant than before.
- new protocol `category` for encoding categories, which are different from cases because they can be set explicitly.
- the registry now contains also the levels or quantiles of the encoded data.

# bitfield 0.7.0

- inclusion of the `project()` helper function that specifies, similarly to `person()`, the project metadata.
- remove automatic creation of a registry when calling `bf_map()` for the sake of more care when users set up a new registry. This should contain author and project metadata.
- instead of storing objects in the environment `bf_env`, they are now made available in the `.GlobalEnv` for easy use.

# bitfield 0.6.0

- rename various things to get a more coherent and intuitive workflow: `bf_test()` -> `bf_map()`, `operator` -> `protocol`, `bf_internal` -> `bf_pcl`
- new function `bf_standards()` to interact with github to pull, push or list community standards base on the github personal access token.
- formalise the bitfield operators into a `protocol`, i.e., a standardised list with a dedicated `.validateProtocol()` function.

# bitfield 0.5.0

- merge all functions into `bf_test()`.
- remove `bf_standards()` and `bf_stds`.
- merge the previous test-functions into a structure that is similar to `bd_stds`, i.e., a list with instructions and documentation for that operation/test (`bf_internal`), which will be called in `bf_test()`.
- revise determination of encoding with more user-friendly interface. Now, everything is specified in `bf_test()` with the `...` argument; documentation explains everything in much more detail.

# bitfield 0.4.0

- include the `.rast()` function for simple handling of gridded objects.
- unit tests for all functions.
- introduction of a small set of standard encodings in `bf_stds` and the function `bf_standard()` to create flags from those.

# bitfield 0.3.0

- major revision of the template function body for the bitfield operators
- more options for bitfield operators, such as the possibility to specify an NA-value and the position in the bitfield, and a custom description
- more stringent provenance handling, now provenance is reported as a combination of the `Action` name and the parameters that are used (following PROV terminology and logic)
- revision of the `bf_decode()` output reporting, now with a more user-friendly return value
- full documentation and functionality of all functions (except auto determination of floating-point encoding for numeric values)
- document also MD5 checksum for registries

# bitfield 0.2.0

- revise various functions
- improve text (readme/vignette)
- change naming of the functions
- correct terminology, the class is not the bitfield but instead an array or registry that contains information about the bitfield. Hence, I renamed it to `registry`
- use attributes to capture a bitflag description and name


# bitfield 0.1.0

- Initial CRAN submission
