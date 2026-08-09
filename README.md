# Distributed Cryptographic Search Framework

An MPI-based manager-worker distributed compute framework built to execute brute-force and table-assisted cryptographic key-space searches across cluster hardware. Implemented using Python 3 and compiled Fortran 90 (`mpifort`) to evaluate dynamic load balancing, task-scheduling efficiency, and search-space reduction metrics.

## Key Capabilities & Architecture

- **Manager-Worker MPI Pipeline:** Dynamic task distribution across a 16-core Raspberry Pi 5 cluster using Open MPI and Python.
- **Dynamic Load Balancing:** Implements demand-driven chunk assignment, achieving a 61% reduction in overall computation time under severe workload imbalance compared to static partitioning.
- **Pluggable Verifier Model:** Decouples core MPI scheduling from cryptographic search primitives, supporting both brute-force evaluation and table-assisted (hash-prefix bucketed) search routines.
- **Hash-Prefix Optimization:** Analyzes search-space reductions across 1-character ($p=1$) and 2-character ($p=2$) hex prefix buckets for SHA-256 digests.

## Repository Structure

- `src/python/`: Python MPI orchestration scripts (`coordinator.py`, `worker.py`, `verifier.py`, `searchspace.py`, `messages.py`)
- `src/fortran/`: Fortran 90 SHA-256 search engine, configuration, and dataset generators (`mpi_search_sha256.f90`, `make_datasets_mpi_cfg.f90`)
- `docs/`: LaTeX technical paper and performance benchmark plots

## Hardware & Software Stack

- **Hardware:** 4x Raspberry Pi 5 compute nodes (16 ARM Cortex-A76 cores @ 2.4 GHz, 8 GB RAM per node) and 1 dedicated head node connected via isolated Gigabit Ethernet VLAN.
- **Software:** PiOS (Linux Kernel 6.12), Open MPI 5.0.7, Python 3.13, `mpifort` 14.2.0.

## Key Performance Results

- **Dynamic Scheduling Efficiency:** Reduced compute times from 876 seconds (static) to 346 seconds (dynamic) on computationally imbalanced workloads.
- **Table-Assisted Search Acceleration:** Achieved a 98.5% to 99.9% runtime reduction compared to brute-force evaluations across evaluated key-space configurations.
