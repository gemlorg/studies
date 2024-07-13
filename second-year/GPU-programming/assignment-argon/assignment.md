# Study of Phase Transitions in Argon Using Molecular Dynamics

The task involves transferring force calculations from CPU to GPU in a program simulating phase transitions of argon.

## Project Parts

1. **Part 1**: Write a C/C++ program to simulate a cluster of argon atoms using the molecular dynamics method.
2. **Part 2**: Transfer the algorithm to GPU using C for CUDA.
3. **Part 3**: Use the program to conduct an interesting simulation of a physical phenomenon using both the scalar version on CPU and the GPU version, and draw conclusions.

## Introduction

The interaction between argon atoms is described by the Lennard-Jones potential:

\[ V*{ij} = \epsilon*{ij} \left( \left( \frac{\sigma*{ij}}{r*{ij}} \right)^{12} - 2 \left( \frac{\sigma*{ij}}{r*{ij}} \right)^6 \right) \]

where \( \sigma*{ij} = \sigma = 0.369 \, \text{nm} \) and \( \epsilon*{ij} = \epsilon = 1.19 \, \text{kJ/mol} \). The distance between atoms \( i \) and \( j \) is \( r\_{ij} \). Since all atoms in the system are identical, \( \sigma \) and \( \epsilon \) are the same for all pairs of atoms.

The total potential energy of the system is:

\[ V = \frac{1}{2} \sum*{i \neq j} V*{ij} = \frac{1}{2} \sum*{i \neq j} \epsilon \left( \left( \frac{\sigma}{r*{ij}} \right)^{12} - 2 \left( \frac{\sigma}{r\_{ij}} \right)^6 \right) \]

The total kinetic energy is:

\[ T = \frac{1}{2} \sum_i m v_i^2 \]

The total energy is:

\[ E = T + V \]

The force vector acting on the i-th atom is given by:

\[ F_i = - \nabla V \]

## Model Setup

For the CPU model, assume a system of 864 atoms arranged on a 6x6x6 elementary cell lattice. For the GPU, assume a system of 4000 argon atoms arranged on a 10x10x10 elementary cell lattice. Each elementary cell contains four argon atoms with the following coordinates:

- \((0.5a, 0.5a, 0.5a)\)
- \((0.5a, 1.5a, 1.5a)\)
- \((1.5a, 0.5a, 1.5a)\)
- \((1.5a, 1.5a, 0.5a)\)

where the unit length \( a \) is given by:

\[ \sigma = 2a \]

## Simulation Algorithm

### Initial Conditions

- Place atoms in lattice nodes.
- Assign initial velocities randomly using a uniform distribution with mean zero and variance such that the average kinetic energy per degree of freedom is \( k_B T \), where \( k_B \) is the Boltzmann constant and \( T \) is the temperature.

### Dynamics

- Use the leap-frog algorithm to integrate the equations of motion.
- Adjust initial velocities to ensure the center of mass velocity is zero.

### Simulation Parameters

- Use the unit system [nm, ps, amu] (nanometers, picoseconds, atomic mass units).
- The Boltzmann constant in these units is \( 8.31 \times 10^{-3} \).

### Stability Condition

- The total energy stability over 10,000 simulation steps with a time step \( t = 0.001 \, \text{ps} \).

## Functional Requirements

1. **Configuration**: Configure through header files or configuration file.
2. **Diagnostics**: Print diagnostics to `argon.out` with varying levels of detail depending on the DEBUG flag.
3. **Energy Output**: Output energy components to `argon_energy.csv`.
4. **Trajectory Output**: Output atom coordinates every N steps to `argon_trajectory.xyz`.

### Part I - Stability Study

- Measure absolute deviation of total energy from the initial value.
- Conduct tests for time steps \( t = 0.002 \), \( 0.005 \), \( 0.010 \), \( 0.020 \), \( 0.050 \) ps at an initial temperature of 70K.

### Optimization

- Algorithmic optimization and compilation options.
- Consider cut-off radius effects on speed and accuracy.

### Part II - GPU Algorithm Transfer

#### Step 1 - Basic Transfer

- Transfer force and energy calculations to GPU, keeping the rest of the simulation on CPU.
- Implement data transfer between CPU and GPU for atom positions and force components.

#### Step 2 - Algorithm Optimization

- Increase parallelism by dividing interactions for each atom among multiple blocks.
- Use shared memory for efficiency.

#### Step 3 - Using Newton's Third Law

- Optimize by computing interactions within each block for host and guest atoms.
- Store partial results in shared memory and aggregate them.

### Part III - Phase Transition Study

- Simulate at constant energy and validate stability.
- Heat the system from 20K to 120K and observe phase transitions.
- Deliver energy by scaling velocities.

### Simplified Simulation Algorithm for Heating

```
BEGIN
Place atoms in lattice nodes.
Assign initial velocities.
Calculate forces on atoms.
for (i = 0; i < n; i++) {
    Propagate using LeapFrog algorithm.
    Scale velocities.
    Update sums, counters, etc.
    Write energy values to file.
    Write expected and actual temperature to file.
    If necessary, write atom coordinates to file.
}
END
```

- Conduct a 20,000-step simulation with a time step of 0.01 ps.
- Analyze results graphically using tools like gnuplot, ggplot, R, or Python.
