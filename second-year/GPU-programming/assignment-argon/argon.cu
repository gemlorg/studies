#include <complex>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <iostream> 

#include "argon.hpp"


__global__ void calc_forces(atoms_state *state, atom *atoms, struct block_results *help) {
    if(blockIdx.x < blockIdx.y) return;
  
  __shared__ coords guests[DIM+1];
  __shared__ forces fguests[DIM+1];
  
  forces myself{};
  atom mycords{};
  real dx, dy, dz, df_by_r2, r2, r6, r12, delj6, delj12, elj6, elj12, sig2, sig4, sig6, sig12;
  int k;
  int natoms = state->natoms;
  int rc2 = state->cutoff * state->cutoff;
  int i =  blockIdx.y*blockDim.y+threadIdx.y;
  int offset = blockIdx.x*blockDim.y;
  block_results *block = &help[blockIdx.y * ((natoms - 1) / DIM + 1) + blockIdx.x];
  
  sig2 = sigma*sigma;
  sig4 = sig2*sig2;
  sig6 = sig2*sig4;
  sig12 = sig6*sig6;

  if(offset + threadIdx.y < natoms) {
    auto import = atoms[offset + threadIdx.y];
    guests[threadIdx.y].x = import.x;
    guests[threadIdx.y].y = import.y;
    guests[threadIdx.y].z = import.z;
    fguests[threadIdx.y].fx = 0;
    fguests[threadIdx.y].fy = 0;
    fguests[threadIdx.y].fz = 0;
  }
  
  __syncthreads();
  if(i < natoms) {mycords = atoms[i];} 
  elj6 = elj12 = 0;
  
  if(state->cutoff != 0) {
    for (int j=0; j<DIM; j++) {
      k = (i+j) % DIM;
      dx = mycords.x - guests[k].x;
      dy = mycords.y - guests[k].y;
      dz = mycords.z - guests[k].z;
      r2 = dx*dx + dy*dy + dz*dz;
      if( k + offset < natoms && k + offset > i && (r2 < rc2 )) {
        r6 = r2*r2*r2;
        r12 = r6*r6;

        delj6 = sig6/r6;
        delj12 = sig12/r12;
        elj6 -= delj6;
        elj12 += delj12;
        df_by_r2 = 12 * epsilon * (delj12 - delj6) / r2;
        myself.fx += df_by_r2 * dx ;
        myself.fy += df_by_r2 * dy ;
        myself.fz += df_by_r2 * dz ;
        fguests[k].fx -= df_by_r2 * dx ;
        fguests[k].fy -= df_by_r2 * dy ;
        fguests[k].fz -= df_by_r2 * dz ;
      }
    __syncthreads();
    }
  }else {
    for (int j=0; j<DIM; j++) {
      k = (i+j) % DIM;
      dx = mycords.x - guests[k].x;
      dy = mycords.y - guests[k].y;
      dz = mycords.z - guests[k].z;
      r2 = dx*dx + dy*dy + dz*dz;
      if(k + offset < natoms && k + offset > i) {
        r6 = r2*r2*r2;
        r12 = r6 * r6;
        delj6 = sig6/r6;
        delj12 = sig12/r12;
        elj6 -= delj6;
        elj12 += delj12;
        df_by_r2 =  (delj12 - delj6) / r2;
        myself.fx += df_by_r2 * dx ;
        myself.fy += df_by_r2 * dy ;
        myself.fz += df_by_r2 * dz ;
        fguests[k].fx -= df_by_r2 * dx ;
        fguests[k].fy -= df_by_r2 * dy ;
        fguests[k].fz -= df_by_r2 * dz ;
      }
      __syncthreads();
    }
  }
  
  if(threadIdx.y + offset < natoms) {
    block->guests[threadIdx.y]= fguests[threadIdx.y];
  }
  if(i < natoms ) {
        block->hosts[threadIdx.y] = myself;
        block->elj_6[threadIdx.y] = elj6;
        block->elj_12[threadIdx.y] = elj12;
      }

}

#define MAXDIM  1000
__global__ void sum_forces(atoms_state *state, atom *atoms, struct block_results *help) {
  __shared__ real  buffer6[MAXDIM]; 
  __shared__ real  buffer12[MAXDIM]; 

  int in_block = threadIdx.y;
  int y =  blockIdx.y;
  int natoms = state->natoms;
  int step = ((natoms - 1) / DIM + 1);
  int num_atom = in_block + blockIdx.y * blockDim.y;
  real sum = 0;

  

  switch(blockIdx.x) {
    case 0:
      for(int x = 0; x < step; x++) {
        if(y <= x) {
          sum += help[y * step + x].hosts[in_block].fx;
        }
        if(y >= x) {
          sum += help[x * step + y].guests[in_block].fx;
        }

      }
      if(num_atom < natoms) {
        atoms[num_atom].fx = 12 * epsilon * sum;
      }
    break;
    case 1:
      for(int x = 0; x < step; x++) {
        if(y <= x) {
          sum += help[y * step + x].hosts[in_block].fy;
        }
        if(y >= x) {
          sum += help[x * step + y].guests[in_block].fy;
        }

      }
      if(num_atom < natoms) {
        atoms[num_atom].fy = 12 * epsilon * sum;
      }
    break;
    case 2:
      for(int x = 0; x < step; x++) {
        if(y <= x) {
          sum += help[y * step + x].hosts[in_block].fz;
        }
        if(y >= x) {
          sum += help[x * step + y].guests[in_block].fz;
        }

      }
      if(num_atom < natoms) {
        atoms[num_atom].fz = 12 * epsilon * sum;
      }
    break;
    case 3:
      for(int x = 0; x < step; x++) {
        if(y <= x) {
          sum += help[y * step + x].elj_6[in_block];
        }

      }
      if(num_atom < natoms) {
        atoms[num_atom].elj_6 = 2 * epsilon * sum;
      }
       
    break;
    case 4:
       for(int x = 0; x < step; x++) {
        if(y <= x) {
          sum += help[y * step + x].elj_12[in_block];
        }

      }
      if(num_atom < natoms) {
        atoms[num_atom].elj_12 = epsilon * sum;
      }     
  }
  



}

void simulation_state::init(int size, real dt, real cutoff, int term, int heat, int cool) {
  cudaError_t status;
  this->dt = dt;
  this->size = size;
  this->natoms = 4 * size * size * size;
  this->cutoff = cutoff;
  this-> term = term;
  this->heat = heat;
  this->cool = cool;
  this->cpu_atoms = new atoms_data;
  this->gpu_atoms = new gpu_atoms_data;
  this->cpu_atoms->atoms = (atom*)calloc(this->natoms , sizeof(atom));
  this->cpu_atoms->astate = new atoms_state;
  this->cpu_atoms->astate->natoms = this->natoms;
  this->cpu_atoms->astate->cutoff = cutoff;

  status = cudaMalloc ((void**)&this->gpu_atoms->atoms , this->natoms * sizeof(atom));
  if (status != cudaSuccess) ERROR(cudaGetErrorString(status));
  status = cudaMalloc ((void**)&this->gpu_atoms->astate , sizeof(atoms_state));
  if (status != cudaSuccess) ERROR(cudaGetErrorString(status));
  status = cudaMalloc ((void**)&this->gpu_atoms->help , pow((this->natoms - 1) / DIM + 1, 2) * sizeof(struct block_results));
  /* printf("size of result %d\n", sizeof(block_results)); */
  if (status != cudaSuccess) ERROR(cudaGetErrorString(status));
}

void simulation_state::build() {

  atom *curr = this->cpu_atoms->atoms;
  real mx=0, my=0, mz=0;
  real box = (0.369*sqrt(2)/2);
  
  for (int ix=0;ix<this->size;ix++) {
    for (int iy=0;iy<this->size;iy++) {
      for (int iz=0;iz<this->size;iz++) {
        curr->x = box*(0.5+ix*2);
        curr->y = box*(0.5+iy*2);
        curr->z = box*(0.5+iz*2);
        curr++;
        curr->x = box*(0.5+ix*2);
        curr->y = box*(1.5+iy*2);
        curr->z = box*(1.5+iz*2);
        curr++;
        curr->x = box*(1.5+ix*2);
        curr->y = box*(0.5+iy*2);
        curr->z = box*(1.5+iz*2);
        curr++;
        curr->x = box*(1.5+ix*2);
        curr->y = box*(1.5+iy*2);
        curr->z = box*(0.5+iz*2);
        curr++;


      }
    }
  }
  curr = this->cpu_atoms->atoms;

  for(int i = 0; i < this->natoms; i++) {
    mx += curr[i].x;
    my += curr[i].y;
    mz += curr[i].z;
  }
  mx /= this->natoms;
  my /= this->natoms;
  mz /= this->natoms;

  for(int i = 0; i < this->natoms; i++) {
    curr->x -= mx;
    curr->y -= my;
    curr->z -= mz;
    curr->fx = 0;
    curr->fy = 0;
    curr->fz = 0;
    curr->vx = 0;
    curr->vy = 0;
    curr->vz = 0;
    curr++;
  }
  /* memset(this->cpu_atoms->astate, 0, sizeof(atoms_state)); */

  DEBUG_PRINTF("build ended!:3\n");
  /* curr = this->cpu_atoms->atoms; */
  /* for(int i = 0; i < this->natoms; i++) { */
  /*   DEBUG_PRINTF("%d: [%.2f %.2f %.2f]\n", i, curr->x, curr->y, curr->z); */
  /*   curr++; */
  /* } */
     

}

void simulation_state::set_random_speeds() {
  real mvx=0, mvy=0, mvz=0, update_kinetic = 0;
  srand(0);
  atom *curr = this->cpu_atoms->atoms;
  real scale = VMAX * 2  ; 
  for(int i = 0; i < this->natoms; i++) {
    curr->vx = scale * (1.0 * rand() / RAND_MAX - 0.5);
    curr->vy = scale * (1.0 * rand() / RAND_MAX - 0.5);
    curr->vz = scale * (1.0 * rand() / RAND_MAX - 0.5);
    mvx += curr->vx;
    mvy += curr->vy;
    mvz += curr->vz;
    curr++;
  }
  mvx /= this->natoms;
  mvy /= this->natoms;
  mvz /= this->natoms;
  curr = this->cpu_atoms->atoms;
  for(int i = 0; i < this->natoms; i++) {
    curr->vx -= mvx;
    curr->vy -= mvy;
    curr->vz -= mvz;
    //DEBUG_PRINTF("v %d: [%f, %f, %f]\n", i, curr->vx, curr->vy, curr->vz);
    update_kinetic += curr->kinetic();
    curr++;
  }
  this->cpu_atoms->astate->e_kin = update_kinetic * m / 2;
  DEBUG_PRINTF("first kinetic is %f\n", this->cpu_atoms->astate->e_kin);
}
void simulation_state::update_half_velocity() {
  for(atom *curr = this->cpu_atoms->atoms; curr < this->cpu_atoms->atoms + this->natoms;curr++) {
    curr->vx += curr->fx * this->dt / m / 2;
    curr->vy += curr->fy * this->dt / m / 2;
    curr->vz += curr->fz * this->dt / m / 2;
  }
}

void simulation_state::update_positions() {
  for(atom *curr = this->cpu_atoms->atoms; curr < this->cpu_atoms->atoms + this->natoms;curr++) {
    curr->x += curr->vx * this->dt;
    curr->y += curr->vy * this->dt;
    curr->z += curr->vz * this->dt;
  }
    /* atom *curr = this->cpu_atoms->atoms; */
  /* DEBUG_PRINTF("up a 0: [%f, %f, %f]\n", curr->x, curr->y, curr->z); */

}

void simulation_state::update_kinetic() {
  real ekin_update = 0;
  for(atom *curr = this->cpu_atoms->atoms; curr < this->cpu_atoms->atoms + this->natoms;curr++) {
    ekin_update += curr->kinetic(); 
  }
  /* for(int i = 10; i < 20; i++) { */
  /*     printf("num %d is ", i); */
  /*     this->cpu_atoms->atoms[i].print(); */
  /**/
  /* } */
  this->cpu_atoms->astate->e_kin = ekin_update * m / 2;


}

void simulation_state::update_potential() {
  real elj_6_update = 0, elj_12_update = 0;
  for(atom *curr = this->cpu_atoms->atoms; curr < this->cpu_atoms->atoms + this->natoms;curr++) {
    elj_6_update += curr->elj_6;
    elj_12_update += curr->elj_12;
  }
  this->cpu_atoms->astate->elj_6 = elj_6_update;
  this->cpu_atoms->astate->elj_12 = elj_12_update;
  this->cpu_atoms->astate->update_potential();

  
}

void simulation_state::move_to_gpu() {
  cudaError_t status;
  status = cudaMemcpy(this->gpu_atoms->atoms, this->cpu_atoms->atoms, this->natoms *  sizeof(atom), cudaMemcpyHostToDevice);
  if (status != cudaSuccess) ERROR(cudaGetErrorString(status));
  status = cudaMemcpy( this->gpu_atoms->astate, this->cpu_atoms->astate, sizeof(atoms_state), cudaMemcpyHostToDevice);
  if (status != cudaSuccess) ERROR(cudaGetErrorString(status));
 
}

void simulation_state::move_to_cpu() {
  cudaError_t status;
  status = cudaMemcpy(this->cpu_atoms->atoms, this->gpu_atoms->atoms, this->natoms *  sizeof(atom), cudaMemcpyDeviceToHost);
  if (status != cudaSuccess) ERROR(cudaGetErrorString(status));
  status = cudaMemcpy( this->cpu_atoms->astate, this->gpu_atoms->astate, sizeof(atoms_state), cudaMemcpyDeviceToHost);
  if (status != cudaSuccess) ERROR(cudaGetErrorString(status));
}

void simulation_state::forces_cpu() {
  atom *first = this->cpu_atoms->atoms;
  real dx, dy, dz, df, r2, r6, r12, delj6, delj12, elj6, elj12, sig2, sig4, sig6, sig12;
  /* clock_t start, end; */

   
  sig2 = sigma*sigma;
  sig4 = sig2*sig2;
  sig6 = sig2*sig4;
  sig12 = sig6*sig6;


  for(int i = 0; i < this->natoms; i++) {
    first[i].fx = first[i].fy = first[i].fz = 0;
  }
  /* start = clock(); */
  for(int i = 0; i < this->natoms; i++) {
    for(int j = i + 1; j < this->natoms; j++) {
       dx = first[i].x - first[j].x;
       dy = first[i].y - first[j].y;
       dz = first[i].z - first[j].z;
       r2 = dx*dx + dy*dy + dz*dz;
       if(r2>=this->cutoff * this->cutoff && this->cutoff != 0) continue;
       r6 = r2*r2*r2;
       r12 = r6*r6;
       delj6 = sig6/r6;
       delj12 = sig12/r12;
       elj6 -= delj6;
       elj12 += delj12;
       df = 12 * epsilon * (delj12 - delj6);
       first[i].fx += df * dx / r2;
       first[j].fx -= df * dx / r2;
       first[i].fy += df * dy / r2;
       first[j].fy -= df * dy / r2;
       first[i].fz += df * dz / r2;
       first[j].fz -= df * dz / r2;
    }
  }
  /* end = clock(); */
  this->cpu_atoms->astate->elj_6 = elj6 * 2 * epsilon;
  this->cpu_atoms->astate->elj_12 = elj12 * epsilon;
  this->cpu_atoms->astate->update_potential();
  /* DEBUG_PRINTF("one step takes %.6f\n", ((double)end - start) * 1000/CLOCKS_PER_SEC); */

}
void simulation_state::baloon_force() {
  atom* first = this->cpu_atoms->atoms;
  real r;
  for(int i = 0; i < this->natoms; i++) {
    r = first[i].radius();

    if(r >= RBALOON) {
      first[i].fx -= (r - RBALOON) * first[i].x / r;
      first[i].fy -= (r - RBALOON) * first[i].y / r;
      first[i].fz -= (r - RBALOON) * first[i].z / r;
      this->cpu_atoms->astate->e_pot += (r - RBALOON) * (r - RBALOON) / 2;

    }
  }
}

void simulation_state::forces_gpu() {
    cudaError_t status;
    dim3 threads(1,DIM,1);
    dim3 blocks((this->natoms-1)/DIM+1,(this->natoms-1)/DIM+1,1);
    clock_t tic, tac, toc;
    tic = clock();
    calc_forces<<<blocks, threads>>>(this->gpu_atoms->astate, this->gpu_atoms->atoms, this->gpu_atoms->help);

    status = cudaDeviceSynchronize();
    if (status != cudaSuccess){    ERROR( cudaGetErrorString(status));}
    tac = clock();
    /* DEBUG_PRINTF("calc_forces took %.6fms\n", ((double)(tac - tic) * 1000 / CLOCKS_PER_SEC)); */

    blocks = dim3(5, (this->natoms-1)/DIM+1);
    sum_forces<<<blocks, threads>>>(this->gpu_atoms->astate, this->gpu_atoms->atoms, this->gpu_atoms->help);
      status = cudaDeviceSynchronize();
    if (status != cudaSuccess){    ERROR( cudaGetErrorString(status));}

    toc = clock();
    /* DEBUG_PRINTF("sum_forces took %.6fms\n", ((double)(toc - tac) * 1000 / CLOCKS_PER_SEC)); */

}

void simulation_state::scale_velocity(real delta) {
  real alpha = 1 + delta / this->cpu_atoms->astate->temp() / this->heat;
  atom *first = this->cpu_atoms->atoms;
  for(int i = 0; i < this->natoms; i++) {
    first[i].vx *= alpha;
    first[i].vy *= alpha;
    first[i].vz *= alpha;
  }


}

void simulation_state::verlet_gpu() {
  this->update_half_velocity();
  this->update_positions();
  this->move_to_gpu();
  this->forces_gpu();
  this->move_to_cpu();
  this->update_potential();
  this->baloon_force();
  this->update_half_velocity();
  this->update_kinetic();

  this->cpu_atoms->astate->update_energy();
}

void simulation_state::verlet_cpu() {
  this->update_half_velocity();
  this->update_positions();
  forces_cpu();
  this->update_half_velocity();
  this->update_kinetic();
  this->cpu_atoms->astate->update_energy();
  /* auto a = this->cpu_atoms->atoms[0]; */
  /* printf("0 a,v,f : [%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f]\n", a.x, a.y, a.z, a.vx, a.vy, a.vz, a.fx, a.fy, a.fz); */
}

void simulation_state::term_step(int step) {
  this->verlet_gpu(); 
}

void simulation_state::heat_step(int step) {
  this->verlet_gpu(); 
  this->scale_velocity(HEAT_DELTA);
}

void simulation_state::simulate() {
  FILE *CSV;
  time_t start, end;
  atoms_state first_state;
  struct atoms_state_stats term_stats; 
  this->build();
  this->set_random_speeds();
  CSV = fopen(Fname,"w");
  fprintf(CSV,"Time,Etot,Epot,Ekin,Elj12,Elj6,Temp\n");
   start = clock();
   this->cpu_atoms->astate->ex = 0;
   this->cpu_atoms->astate->exx = 0;
  for(int step = 0; step < this->term; step++) {
    this->term_step(step);
    if(step == 0)  first_state = std::move(*this->cpu_atoms->astate);
    if(step % SAVE == 0) this->cpu_atoms->astate->print_csv(CSV, step, this->dt, this->natoms);
    this->cpu_atoms->astate->update_stats();
    
  }

  end = clock();
  real term_time = ((real)(end - start )) / CLOCKS_PER_SEC;
   if(this->term > 0) term_stats = this->cpu_atoms->astate->get_stats(this->term, first_state);
  start = clock(); 
  for(int step = 0; step < this->heat; step++) {
    this->heat_step(step);
    if(step == 0)  first_state = std::move(*this->cpu_atoms->astate);
    if((step + this->term) % SAVE == 0) this->cpu_atoms->astate->print_csv(CSV, step + this->term, this->dt, this->natoms);
    this->cpu_atoms->astate->update_stats();
    
  }
  end = clock();
  printf("term took %.6f seconds\n", term_time);
  printf("term stats: e_tot_delta=%.2f e_tot_std=%.2f\n", term_stats.e_total_delta, term_stats.e_total_std);
  printf("%.3f & %.2f & %.2f  \\\\ \n", this->dt, term_stats.e_total_delta, term_stats.e_total_std);



  printf("heat took %.6f seconds\n", ((real)(end - start )) / CLOCKS_PER_SEC);


  fclose(CSV);

}


simulation_state state;


int main(int argc, char** argv){
    int  size, term, heat, cool;
    real dt, cutoff;
    
    if (argc!=7) {
      printf("Usage: %s SIZE STEP CUTOFF TERM HEAT COOL\n",argv[0]);
      exit(1);
    } else {
      size=atoi(argv[1]);
      dt=atof(argv[2]);
      cutoff=atof(argv[3]);
      term=atoi(argv[4]);
      if (argc>4) {
          heat=atoi(argv[5]);
          cool = atoi(argv[6]);
      }
    }
    state.init(size, dt, cutoff, term, heat, cool);
    state.simulate(); 
    
}

