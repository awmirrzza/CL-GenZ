# Implementation and Evaluation of sequent-calculus-based Coalition Logic for GenZ

-The repository contains not only the Implementation but also Evaluation of Coalition logic in the GenZ prover.

## Credits
GenZ was created by Xiaoshuang Yang:
https://github.com/XiaoshuangYang999/GenZ

The modified version and GenZ project are shared under the GNU General Public License version 3. See `LICENSE`.

## My contribution
- Coalition Logic syntax, input check and proof rules
- correctness check using GenZ and as an additional check, GenT was further used.
- 11 edge-case checks
- seven benchmark families
- two additional families that has not yet been evaluated specifically targeting CL Left and AG Pi respectively
- runtime measured with optimized `-O2`
- a separate optimized `-O2` post-hoc doubling experiment



## Added files

- `lib/Logic/Coalition/Amir_CL.hs`
- `evaluation/`
- `lib/Logic/Coalition/Evaluation/Test/Correctness.hs`
- `lib/Logic/Coalition/Evaluation/EdgeCases.hs`
- `lib/Logic/Coalition/Evaluation/Benchmark/`
- `lib/Logic/Coalition/Evaluation/PostHoc/`


## Evidence
the optimized post-hoc  and the runtime have been both compiled with GHC `-O2`.There is an earlier post-hoc dataset that was produced through GHCi which kept as an evidence and no files have overwritten one or the other.

