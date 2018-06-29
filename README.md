# DKMeta

DKMeta is a wrapper around Dedukti that normalize a Dedukti file according to a set of user-defined *meta* rules. These rules can be disjoint of the rules used to type check the file.

For the moment, all the meta rules should be well-typed but this might change in the future.

DKMeta aims also to provide a way to reflect the Dedukti terms in the syntax of the meta rules. To achieve this goal, DKMeta use encodings. This allows the user to rewrite a dependent product if he wants to (this is not possible in Dedukti itself).
