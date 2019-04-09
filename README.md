# narrator

Narrator Nomenclature

Facts, subject

  *  fact
an assertion, logically homologous to a triple of the form
<br>\<subject relation object\> 
<br>  —or—
<br>\<subject property value\>

  *  fact-base
a background database of ALL facts, such as a triple store

  *  subject-id
universal identifier for the subject of interest for a narrative (presumably DUNS ID)

  *  subject-facts
a source for all pacts pertaining to a particular subject of interest

this might be either just a degenerate plist, an actual database of some kind, or the general fact-base with a subject-specific wrapper around accesses


Narrative

  * narrative
This is a general narrative that may be rendered about a subject.  It consists of a sequence of templates that may be rendered with respect to the facts of the particular subject

•	render-narrative (narrative facts)
produces a string (raw, HTML, ?) which specializes the generic sequence of templates in the narrative using the supplied facts.  

Effectively iterates over narrative sequence calling render-template with given facts

•	render-termplate (template facts)
produces a string suitable for use by render-narrative for a given template


Proposal, Info, extensibility

•	proposal

A proposal is a narrative augmented by some proposal-info (eg facts “consumed” by the proposal)

•	proposal-viable
Predicate to determine if proposal is potentially renderrable.  False implies that something is wrong with proposal (eg requires use of unavailable facts).  A proposal that is NOT viable is a definite “fail”.

•	proposal-extensible
Predicate that determines if proposal can potentially be augmented to produce another proposal (eg one that uses additional facts)

•	proposal-usable
Predicate that determines whether proposal is usable as-is, without requiring additional extension.  A proposal that it usable is a definite “success”.

Note: a proposal that is NOT viable can NOT be either usable nor extensible.  A usable proposal can be either used as-is or it MAY be extended.







