## Requirements for MVP

- Implement backtracking for recursive procedure calls

## Requirements for program verification

- Track/report errors detected by the analysis
- Check contracts when calling primitive procedures

## Nice-to-haves

- Limit evaluation time for unit tests
- Implement fixpoint iteration more efficiently (i.e. avoid unless necessary)
- Make the algorithm generic in terms of the analysis domain
    - Parameterize with procedures like `syntax->value`, `<=?` and `lub`
    - Replace `make-base-environment` with `syntax->value`?
- Split monolithic analysis domain into smaller, simpler domains
    - Domain of constants (i.e. flat domain)
    - Domain of types
    - Domain composition? (e.g. constants + types)
- Implement more primitive procedures
- Check performance impact of using contracts?
- Replace streams with exceptions for error handling?
