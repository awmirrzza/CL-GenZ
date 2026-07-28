# Implementation and Evaluation of Sequent-Calculus-Based Coalition Logic for GenZ

This repository contains the implementation and evaluation of Coalition
Logic in the GenZ prover. It was developed for the thesis
**A Sequent Calculus for Coalition Logic in GenZ**.

The project adds Coalition Logic to GenZ while reusing its existing
sequent representation and proof-search procedures.

A detailed guide connecting the thesis results to their supporting
source code, data, and figures is available in the
[thesis implementation and evaluation guide](./lib/Logic/Coalition/README.md).

## Credits

This project is based on the original GenZ repository maintained by
Xiaoshuang Yang:

<https://github.com/XiaoshuangYang999/GenZ>

The original GenZ README is preserved in
[`README-UPSTREAM.md`](./README-UPSTREAM.md).

This modified repository is distributed under the GNU General Public
License version 3. See [`LICENSE`](./LICENSE).

## Contributions of this thesis

The following components were added as part of the thesis:

- Coalition Logic syntax and formula representation;
- agents, coalitions, and configurable agent universes;
- input well-formedness checking;
- the coalition-left and coalition-right proof rules;
- twelve correctness cases evaluated with GenZ and GenT;
- eleven edge-case checks;
- seven parameterised runtime benchmark families;
- structural measurements for the benchmark formulas;
- a predefined runtime experiment compiled with GHC using `-O2`;
- a separate post-hoc doubling experiment compiled with GHC using
  `-O2`; and
- archived source code, experimental settings, raw outputs, processed
  results, and figures.

GenT was used as an additional check in the correctness evaluation.
However, GenZ and GenT use the same Coalition Logic rule
implementation. The independently expected answers are supported by the
manual SCL derivations and non-derivability analyses presented in the
thesis.

## Main files

- [`lib/Logic/Coalition/Amir_CL.hs`](./lib/Logic/Coalition/Amir_CL.hs):
  main Coalition Logic implementation;
- [`lib/Logic/Coalition/Evaluation/Test/Correctness.hs`](./lib/Logic/Coalition/Evaluation/Test/Correctness.hs):
  twelve correctness cases;
- [`lib/Logic/Coalition/Evaluation/EdgeCases.hs`](./lib/Logic/Coalition/Evaluation/EdgeCases.hs):
  eleven edge-case checks;
- [`lib/Logic/Coalition/Evaluation/Benchmark`](./lib/Logic/Coalition/Evaluation/Benchmark/):
  benchmark families, structural measurements, and predefined runtime
  program; and
- [`lib/Logic/Coalition/Evaluation/PostHoc`](./lib/Logic/Coalition/Evaluation/PostHoc/):
  post-hoc evaluation programs.

## Evaluation evidence

The final correctness and edge-case outputs are:

- [`evaluation/correctness-output-final-2026-07-23.csv`](./evaluation/correctness-output-final-2026-07-23.csv);
- [`evaluation/edge-output-final-2026-07-23.csv`](./evaluation/edge-output-final-2026-07-23.csv).

All twelve correctness cases and all eleven edge-case checks produced
their expected results.

The files for the predefined runtime experiment are available in
[`evaluation/raw`](./evaluation/raw/). The figures are available in
[`evaluation/figures`](./evaluation/figures/).

## Final post-hoc evidence

The files used for the final post-hoc results are:

- [`evaluation/PostHoc/optimized-2026-07-18-source`](./evaluation/PostHoc/optimized-2026-07-18-source/);
- [`evaluation/PostHoc/double-runtime-2nd-2026-07-18.csv`](./evaluation/PostHoc/double-runtime-2nd-2026-07-18.csv);
- [`evaluation/PostHoc/double-runtime-2nd-o2-medians-2026-07-18.csv`](./evaluation/PostHoc/double-runtime-2nd-o2-medians-2026-07-18.csv);
- [`evaluation/PostHoc/double-runtime-2nd-o2-metadata-2026-07-18.txt`](./evaluation/PostHoc/double-runtime-2nd-o2-metadata-2026-07-18.txt); and
- [`evaluation/PostHoc/optimized-2026-07-18-environment`](./evaluation/PostHoc/optimized-2026-07-18-environment/).

An earlier post-hoc dataset produced through GHCi is preserved for
provenance. It was not used for the final thesis results and must not be
combined with the optimized 18 July dataset.

## Evaluated repository version

The implementation and archived evaluation files used for the thesis
are preserved in commit
[`cc8f36e`](https://github.com/awmirrzza/CL-GenZ/commit/cc8f36e6722d309595684941c54f7c6a643a134f).

This README may be added in a later commit, but `cc8f36e` remains the
fixed snapshot of the implementation and evaluation artifacts used for
the thesis.
