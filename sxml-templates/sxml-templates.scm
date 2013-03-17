(module
 sxml-templates
 (make-sxml-template
  sxml-template?
  sxml-template:sxml
  sxml-template:transformed-sxml
  sxml-template:filled?
  sxml-template:fill
  sxml-template:fill-string
  sxml-template->string)
 (import scheme chicken)
 (use sxml-transforms
      records
      data-structures
      ports)
 (include "sxml-templates-implementation.scm"))
