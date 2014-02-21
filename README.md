OOCFA2
======

This is a public view of my Master's thesis work.  Unfortunately, at the moment it cannot be built without a few invasive changes to the Android codebase and without using Eclipse to bridge the Android buildsystem and this one.  Until I've had the opportunity to change this, the purpose of this repository is simply to expose the work for viewing.

Abstract
------

The application of higher-order PDA-based flow analyses to object-oriented languages enables comprehensive and precise characterization of program behavior, while retaining practicality with efficiency. We implement one such flow analysis which we've named OOCFA2.

While over the years many advancements in flow analysis have been made, they have almost exclusively been with respect to functional languages, often modeled with the λ-calculus. Object-oriented semantics — while also able to be modeled in a functional setting — provide certain structural guarantees and common idioms which we believe are valuable to reason over in a first-class manner. By tailoring modern, advanced flow analyses to object-oriented semantics, we believe it is possible to achieve greater precision and efficiency than could be had using a functional modeling. This, in turn, reflects upon the possible classes of higher-level analyses using the underlying flow analysis: the more powerful, efficient, and flexible the flow analysis, the more classes of higher-level analyses — e.g., security analyses — can be practically expressed.

The growing trend is that smartphone and mobile-device (e.g., tablet) users are integrating these devices into their lives, in more frequent and more personal ways. Accordingly, the primary application and proof-of-concept for this work is the analysis of the Android operating system's permissions-based security system vis-à-vis potentially malicious applications. It is implemented atop OOCFA2. The use of a such a powerful higher-order flow analysis allows one to apply its knowledge to create a wide variety of powerful and practical security-analysis “front-ends” — not only the permissions-checking analysis in this work, but also, e.g., information-flow analyses.
