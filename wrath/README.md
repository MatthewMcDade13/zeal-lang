# Wrath Lisp Dialect

### Notes / Wants
- Zeal code will translate to Wrath AST 
- Wrath is its own standalone interpreter for running fast scripts
- Wrath can be compiled to bytecode for VM


### Considerations
- Where is compiler going to live? 
  - Zeal
  - Wrath
  - Standalone project


- Possible translation pipelines:
  - Zeal -> Wrath -> VM
  - Zeal -> Wrath -> VM | Zeal -> VM

`Makes more sense to use at least a minimal/lower level version of wrath to use for AST of Zeal, maybe something like 'WrathIR'?`
