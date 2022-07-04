
<p align="center">
  <img width="500" height="100%" src="./docs/img/logo-text-colored-new.png">
</p>

# The open GOAL type system for Racket

The implementation of the GOAL type system with Racket language. This explanation of the GOAL type system available [here](https://open-goal.github.io/docs/reference/type_system) Or you can see the source code of whole [Jack Project](https://github.com/open-goal/jak-project)

## Files

- interfaces.rkt -- Interface for all types
- type.rkt -- The base type for all types
- basic-types.rkt -- All avaibale types: field, null, struct, value, bitfield, enum
- type-spec.rkt -- The universal type reference aka '(function (int int int))'
- state.rkt -- The state type functions
- type-system.rkt -- The main type system 
- builting-types.rkt -- Initializer of type system by builting types
- defenum.rkt -- The enum expression parser
- deftype.rkt -- The deftype expression parser
- rt-type.rkt -- The runtime version of basic type. Not used by type system above
- goalc-all-types.gc -- The list of typespecs of the GOAL
- goalc-type-specs.txt -- The GOAL types file used for testing
