# Sequent-Calculus-Based Coalition Logic for GenZ

This repository contains my implementation and evaluation of Coalition
Logic in GenZ. The work was completed for the thesis
**A Sequent Calculus for Coalition Logic in GenZ**.

I added Coalition Logic without changing GenZ's existing proof-search
procedures.

## Credits

This project is based on the original GenZ repository maintained by
Xiaoshuang Yang:

<https://github.com/XiaoshuangYang999/GenZ>

The original GenZ README is available in
[`README-UPSTREAM.md`](./README-UPSTREAM.md).

This repository uses the GNU General Public License version 3. See
[`LICENSE`](./LICENSE).

## What I added

I added:

- the syntax of Coalition Logic;
- agents, coalitions, and configurable agent universes;
- a check that every coalition belongs to the selected agent universe;
- the coalition-left and coalition-right proof rules;
- twelve correctness cases;
- eleven edge-case checks;
- seven runtime benchmark families;
- measurements of the benchmark formulas;
- a main runtime experiment compiled with GHC `-O2`;
- a post-hoc experiment compiled with GHC `-O2` and without GHC `-O2`; and
- the source code, results, settings, analysis files, and figures.

## Main files

The main Coalition Logic implementation is:

- [`Amir_CL.hs`](./lib/Logic/Coalition/Amir_CL.hs)

The evaluation programs are:

- [`Correctness.hs`](./lib/Logic/Coalition/Evaluation/Test/Correctness.hs)
- [`EdgeCases.hs`](./lib/Logic/Coalition/Evaluation/EdgeCases.hs)
- [`BenchmarkFamilies.hs`](./lib/Logic/Coalition/Evaluation/Benchmark/BenchmarkFamilies.hs)
- [`BenchmarkProperties.hs`](./lib/Logic/Coalition/Evaluation/Benchmark/BenchmarkProperties.hs)
- [`Runtime.hs`](./lib/Logic/Coalition/Evaluation/Benchmark/Runtime.hs)
- [`PostHoc`](./lib/Logic/Coalition/Evaluation/PostHoc/)

## Correctness results

The correctness evaluation contains twelve cases for:

- liveness;
- safety;
- outcome monotonicity;
- superadditivity;
- grand-coalition maximality; and
- coalition monotonicity.

The final output is:

[`correctness-output-final-2026-07-23.csv`](./evaluation/correctness-output-final-2026-07-23.csv)

The six derivable cases returned `True`, and the six non-derivable cases
returned `False`. GenZ and GenT returned the expected answer in all
twelve cases.

GenT was used as an additional check. However, GenZ and GenT use the
same Coalition Logic rules. The manual SCL proofs and non-derivability
analyses in the thesis provide the independent expected answers.

## Edge-case results

The edge-case evaluation checks important rule conditions, including:

- disjoint and overlapping coalitions;
- the empty coalition;
- the `m >= 1` and `n = 0` cases;
- `B = Ag`;
- `B` equal to the empty coalition;
- an unboxed atom; and
- a coalition outside the agent universe.

The final output is:

[`edge-output-final-2026-07-23.csv`](./evaluation/edge-output-final-2026-07-23.csv)

All eleven checks produced the expected result.

Case c2 checks that the coalition-left rule does not apply to an unboxed
atom. Case c11 checks that an invalid coalition is rejected before proof
search.

## Main runtime experiment

The runtime experiment uses seven formula families:

1. superadditivity;
2. non-valid superadditivity;
3. outcome monotonicity;
4. coalition monotonicity;
5. nested `[C]` formulas;
6. chain; and
7. nested chain.

The main result files are:

- [`runtime-results.csv`](./evaluation/raw/runtime-results.csv) — raw
  results;
- [`runtime-medians-seconds.csv`](./evaluation/raw/runtime-medians-seconds.csv)
  — median runtimes in seconds;
- [`runtime-timeouts.csv`](./evaluation/raw/runtime-timeouts.csv) — the
  timeout cases; and
- [`runtime-analysis.xlsx`](./evaluation/runtime-analysis.xlsx) — the
  analysis workbook.

The experiment was compiled with GHC using `-O2`. Each completed formula
was run three times, and the median was used. Each run had a 100-second
timeout.

A timeout means that GenZ did not return an answer within 100 seconds.
It does not mean that the formula was non-derivable.

The experiment settings and computer information are available in
[`evaluation/raw`](./evaluation/raw/).

## Final post-hoc experiment

The post-hoc experiment tested coalition monotonicity, outcome
monotonicity, and nested `[C]` formulas at larger values.

The final experiment was compiled with GHC using `-O2`. Its files are:

- [`optimized-2026-07-18-source`](./evaluation/PostHoc/optimized-2026-07-18-source/)
  — the source files used for the experiment;
- [`double-runtime-2nd-2026-07-18.csv`](./evaluation/PostHoc/double-runtime-2nd-2026-07-18.csv)
  — the raw results;
- [`double-runtime-2nd-o2-medians-2026-07-18.csv`](./evaluation/PostHoc/double-runtime-2nd-o2-medians-2026-07-18.csv)
  — the final medians;
- [`double-runtime-2nd-o2-metadata-2026-07-18.txt`](./evaluation/PostHoc/double-runtime-2nd-o2-metadata-2026-07-18.txt)
  — information about the experiment; and
- [`optimized-2026-07-18-environment`](./evaluation/PostHoc/optimized-2026-07-18-environment/)
  — the compilation and computer information.

An earlier post-hoc dataset produced through GHCi is also kept in the
repository. It was not used for the final thesis results. The earlier
and final datasets should not be combined.

## Figures

The thesis figures are stored in
[`evaluation/figures`](./evaluation/figures/):

- [`Fig1_runtime_vs_n.pdf`](./evaluation/figures/Fig1_runtime_vs_n.pdf)
- [`fig2_posthoc.pdf`](./evaluation/figures/fig2_posthoc.pdf)
- [`fig3a_formula_size.pdf`](./evaluation/figures/fig3a_formula_size.pdf)
- [`fig3b_c_count.pdf`](./evaluation/figures/fig3b_c_count.pdf)
- [`fig3c_nesting_levels.pdf`](./evaluation/figures/fig3c_nesting_levels.pdf)
- [`fig3d_sum_coalition_sizes.pdf`](./evaluation/figures/fig3d_sum_coalition_sizes.pdf)
- [`FinalFigures.xlsx`](./evaluation/figures/FinalFigures.xlsx)

## Meaning of the results

- In the `expected`, `genz`, and `genT` columns, `True` means derivable
  and `False` means non-derivable.
- In a validity column, `True` means that the formula passed the input
  check.
- In an agreement column, `True` means that the result was the same as
  the expected result.
- `Error` means that the input was rejected before proof search.
- `Timeout` means that GenZ did not return an answer within 100 seconds.

## Limits of the results

The correctness and edge-case results support the tested cases. They do
not prove that the implementation is correct for every possible
Coalition Logic formula.

The runtime results apply to the tested families and ranges. They do not
show the general runtime complexity of GenZ.

## Thesis version

The implementation and evaluation files used in the thesis are stored
in commit
[`cc8f36e`](https://github.com/awmirrzza/CL-GenZ/commit/cc8f36e6722d309595684941c54f7c6a643a134f).

The README may be added in a later commit, but `cc8f36e` remains the
fixed version used for the thesis evaluation.
