# bitfield 0.4.0

- include the `.rast()` function for simple handling of gridded objects.
- unit tests for all functions.
- introduction of a small set of standard encodings in `bf_stds` and the function `bf_standards()` to create flags from those.


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
