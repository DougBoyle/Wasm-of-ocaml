#include <math.h>

const double pi = 3.141592653589793;
const double solar_mass = 4.0 * pi * pi;
const double days_per_year = 365.24;

// Should really pay closure attention to use of const/non-const in other C programs, affects optimisations
struct planet {
    float x, y, z, vx, vy, vz;
    const float mass;
};
typedef struct planet Planet;

void advance(Planet **bodies, int num_bodies, double dt){
    for (int i = 0; i < num_bodies; i++){
        Planet *b = bodies[i];
        for (int j = i + 1; j < num_bodies; j++){
            Planet *b2 = bodies[j];
            double dx = b->x - b2->x, dy = b->y - b2->y, dz = b->z - b2->z;
            double dist2 = dx * dx + dy * dy + dz * dz;
            double mag = dt/(dist2 * sqrt(dist2));

            b->vx -= dx * b2->mass * mag;
            b->vy -= dy * b2->mass * mag;
            b->vz -= dz * b2->mass * mag;

            b2->vx += dx * b->mass * mag;
            b2->vy += dy * b->mass * mag;
            b2->vz += dz * b->mass * mag;
        }
    }
    for (int i = 0; i < num_bodies; i++){
        Planet *b = bodies[i];
        b->x += dt * b->vx;
        b->y += dt * b->vy;
        b->z += dt * b->vz;
    }
}

double energy(Planet **bodies, int num_bodies){
  double e = 0;
  for (int i = 0; i < num_bodies; i++){
    Planet *b = bodies[i];
    e += 0.5 * b->mass * (b->vx * b->vx + b->vy * b->vy + b->vz * b->vz);
    for (int j = i + 1; j < num_bodies; j++){
      Planet *b2 = bodies[j];
      double dx = b->x - b2->x, dy = b->y - b2->y, dz = b->z - b2->z;
      double distance = sqrt(dx * dx + dy * dy + dz * dz);
      e -= (b->mass * b2->mass) / distance;
    }
  }
  return e;
}

void offset_momentum(Planet **bodies, int num_bodies){
  double px = 0, py = 0, pz = 0;
  for (int i = 0; i < num_bodies; i++){
    px += bodies[i]->vx * bodies[i]->mass;
    py += bodies[i]->vy * bodies[i]->mass;
    pz += bodies[i]->vz * bodies[i]->mass;
  }
  bodies[0]->vx = - px / solar_mass;
  bodies[0]->vy = - py / solar_mass;
  bodies[0]->vz = - pz / solar_mass;
}

Planet jupiter = {4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
    1.66007664274403694e-03 * days_per_year, 7.69901118419740425e-03 * days_per_year,
    -6.90460016972063023e-05 * days_per_year, 9.54791938424326609e-04 * solar_mass};

Planet saturn = {  8.34336671824457987e+00,
                4.12479856412430479e+00,
                -4.03523417114321381e-01,
                -2.76742510726862411e-03 * days_per_year,
                4.99852801234917238e-03 * days_per_year,
                2.30417297573763929e-05 * days_per_year,
               2.85885980666130812e-04 * solar_mass    };

Planet uranus = {  1.28943695621391310e+01,
                -1.51111514016986312e+01,
                -2.23307578892655734e-01,
                2.96460137564761618e-03 * days_per_year,
                2.37847173959480950e-03 * days_per_year,
                -2.96589568540237556e-05 * days_per_year,
               4.36624404335156298e-05 * solar_mass     };

Planet neptune = {  1.53796971148509165e+01,
                 -2.59193146099879641e+01,
                 1.79258772950371181e-01,
                 2.68067772490389322e-03 * days_per_year,
                 1.62824170038242295e-03 * days_per_year,
                 -9.51592254519715870e-05 * days_per_year,
                5.15138902046611451e-05 * solar_mass   };

Planet sun = {  0.,   0.,   0.,   0.,   0.,  0., solar_mass };

int main(){
    Planet *bodies[] = {&sun, &jupiter, &saturn, &uranus, &neptune};
    int num_bodies = 5;
    int n = 100;
    double e = 0.;
    offset_momentum(bodies, num_bodies);
    e = energy(bodies, num_bodies);
    for (int i = 0; i <= n; i++){
        advance(bodies, num_bodies, 0.01);
    }
    e += energy(bodies, num_bodies);
    // to stop it being optimised away
    return (int)e;
}