#include <iostream>
#include <cmath>

#define real float 

#define epsilon 1.19
#define sigma 0.369
#define kB 8.31e-3
#define m 39.95
#define DEBUG 0
#define Term 500
#define Heat 0
#define Cool 0
#define T0 20
#define SAVE 10
#define Fname "energy.csv"
#define B 1.0 
#define Rb 10.0
#define RBALOON 10
#define VMAX sqrt(3*kB*T0/m)
#define HEAT_DELTA 200


#define DIM 64
#if((defined(DEBUG))&&(DEBUG>0)) 
    #define DEBUG_PRINTF(...) printf("DEBUG: " __VA_ARGS__)
#else
    #define DEBUG_PRINTF(...) do {} while (0)
#endif

#define ERROR(s) do{perror(s);exit(1);} while(0)

typedef struct gpu_atoms_data gpu_atoms_data;
typedef struct atoms_data atoms_data;
typedef struct simulation_state simulation_state;
typedef struct atoms_state atoms_state;
typedef struct atom atom;
typedef struct coords coords ;
typedef struct forces forces;
typedef struct block_results block_results;


struct forces {
  real fx;
  real fy;
  real fz;
};
struct block_results {
  forces guests[DIM];
  forces hosts[DIM];
  real elj_6[DIM];
  real elj_12[DIM];
};

struct atom {
  real x;
  real y;
  real z;
  real vx;
  real vy;
  real vz;
  real fx;
  real fy;
  real fz;
  real elj_6;
  real elj_12;

  real kinetic() {
    return vx*vx + vy*vy + vz*vz;
  }

  void print() {
    printf("fx fy fz elj6 elj12 : [%.2f, %.2f, %.2f, %.2f, %.2f]\n", fx, fy, fz,  elj_6, elj_12);
  }
  real radius() {
    return sqrt( x * x + y * y + z * z);
  }
  
};

struct atoms_state_stats {
  real e_total_start;
  real e_total_end;
  real e_total_delta;
  real e_total_exp;
  real e_total_var;
  real e_total_std;
  real e_kin_exp;
  real e_kin_std;
  real e_pot_exp;
  real e_pot_std;
};

struct atoms_state {
  int natoms;
  real cutoff;
  real e_total;
  real e_kin;
  real e_pot;
  real elj_6;
  real elj_12;
  double ex, exx, e_kinx, e_kinxx, e_potx, e_potxx;

  struct atoms_state_stats get_stats(int n, atoms_state begin) {
    struct atoms_state_stats res;
    res.e_total_start = begin.e_total;
    res.e_total_end = this->e_total;
    res.e_total_delta = abs(begin.e_total - this->e_total);
    res.e_total_exp = this->ex / n;
    res.e_total_var = this->exx / n - res.e_total_exp * res.e_total_exp;
    res.e_total_std = sqrt(fabs(res.e_total_var));
    res.e_kin_exp = this->e_kinx / n;
    res.e_kin_std = sqrt((this->e_kinxx   - res.e_kin_exp * res.e_kin_exp)/n);
    res.e_pot_exp = this->e_potx / n;
    res.e_pot_std = this->e_potxx / n - res.e_pot_exp * res.e_pot_exp;
    return res;

  }

  
  void print_csv(FILE* CSV, int step, real dt, int natoms) {
    real temp = this->temp();
    fprintf(CSV,"%lf, %lf, %lf, %lf, %lf, %lf, %lf\n",step * dt ,e_total,e_pot,e_kin,elj_12,elj_6,temp);
    printf("Step %i, ETOT = %lf \n",step, e_total);
  }
  real temp() {
    return e_kin / natoms * 2 / 3 / kB;
  }
  void update_energy() {
    e_total = e_kin + e_pot;
  }
  void update_potential() {
    e_pot = elj_6 + elj_12;
  }
  void get_info() {
    printf("e_tot=%.4f e_kin=%.4f e_pot=%.4f elj_6=%.4f elj_12=%.4f\n", e_total, e_kin, e_pot, elj_6, elj_12);
  }
  void update_stats() {
    ex += e_total;
    exx += e_total * e_total;
    e_kinx += e_kinx;
    e_kinxx += e_kinx * e_kinx;
    e_potx += e_pot;
    e_potxx += e_pot * e_pot;

  }
};

struct atoms_data {
  atom *atoms;
  atoms_state *astate;
};

struct gpu_atoms_data {
  atom *atoms;
  atoms_state *astate;
  block_results *help;
};

struct simulation_state {
  atoms_data * cpu_atoms;
  gpu_atoms_data * gpu_atoms;

  int size;
  int natoms;
  int term;
  int heat;
  int cool;
  real cutoff;
  real dt;
  

  void init(int size, real dt, real cutoff, int term, int heat, int cool);
  void build();
  void baloon_force();
  void simulate();
  void set_random_speeds();
  void scale_velocity(real delta);
  void term_step(int step);
  void heat_step(int step);
  void verlet_gpu();
  void verlet_cpu();
  void update_half_velocity();
  void update_positions();
  void move_to_gpu();
  void move_to_cpu();
  void update_kinetic();
  void update_potential();
  void forces_cpu();
  void forces_gpu();
  
};

struct coords {
  real x;
  real y;
  real z;
  
};
