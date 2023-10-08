## Requirements for MVP

- Implement backtracking for recursive procedure calls

## Requirements for program verification

- Track/report errors detected by the analysis
- Check contracts when calling primitive procedures

## Nice-to-haves

- Limit evaluation time for unit tests
- Implement fixpoint iteration more efficiently (i.e. avoid unless necessary)
- Replace `make-base-environment` with `syntax->value`?
- Split monolithic analysis domain into smaller, simpler domains
    - Domain of constants (i.e. flat domain)
    - Domain of types
    - Domain composition? (e.g. constants + types)
- Implement more primitive procedures
- Check performance impact of using contracts?
- Replace streams with exceptions for error handling?
- Make it so higher-order primitive procedures work with the analysis?
    - Make closures callable using `prop:procedure`
    - Pass the `trace` argument around implicitly as a parameter
