#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <limits>
#include <algorithm>
#include <locale.h>
#include <chrono>
#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <limits>
#include <algorithm>
#include <random>

typedef long double dl;
typedef long long ll;

using namespace std;

std::random_device rd;
std::mt19937 gen(rd());

#define MAXPOP	100

struct gene {
	dl alleles[3];
	ll fitness;
	dl likelihood;

	// Test for equality.
	bool operator==(gene gn) {
		for (ll i = 0; i < 3; i++) {
			if (gn.alleles[i] != alleles[i]) {
				return false;
			}
		}
		return true;
	}
};

class Parabola {
public:
	Parabola(ll n, vector<ll>&, vector<ll>&);// Constructor with coefficients for a,b,c.
	ll Solve();// Solve the equation.

	// Returns a given gene.
	gene GetGene(ll i) {
		return population[i];
	}

	vector <ll> x, y;// The coefficients.
	ll n;
	gene population[MAXPOP];// Population.

	dl Fitness(gene&);// Fitness function.
	void GenerateLikelihoods();	// Generate likelihoods.
	dl MultInv();// Creates the multiplicative inverse.
	ll CreateFitnesses();
	void CreateNewPopulation();
	ll GetIndex(dl val);

	gene Breed(ll p1, ll p2);

};

Parabola::Parabola(ll n_, vector<ll>& x_, vector<ll>& y_) : x(x_), y(y_), n(n_) {}

ll Parabola::Solve() {
	ll fitness = -1;

	// Generate initial population.

	for (ll i = 0; i < MAXPOP; i++) {// Fill the population with numbers between
		for (ll j = 0; j < 3; j++) {// 0 and the result.
			population[i].alleles[j] = (dl)gen() / RAND_MAX / RAND_MAX * 5 - 10;
		}
	}

	fitness = CreateFitnesses();
	if (fitness != 0) {
		return fitness;
	}
	ll iterations = 0;// Keep record of the iterations.
	while (iterations < 1) {// Repeat until solution found
		//cout << iterations << "\n";
		GenerateLikelihoods();// Create the likelihoods.
		CreateNewPopulation();
		fitness = CreateFitnesses();
		if (fitness != 0) {
			return fitness;
		}
		iterations++;
	}
}

dl ans = 1e18;
gene best;

dl Parabola::Fitness(gene& gn) {
	dl total = 0;
	for (ll i = 0; i < n; i++) {
		//cout << gn.alleles[0] << " " << gn.alleles[1] << " " << gn.alleles[2] << "\n";
		total += abs(y[i] - (gn.alleles[0] * x[i] * x[i] + gn.alleles[1] * x[i] + gn.alleles[2]));
	}
	if (total < ans) {
		ans = total;
		best = gn;
	}
	return total;
}

ll Parabola::CreateFitnesses() {
	dl avgfit = 0;
	for (ll i = 0; i < MAXPOP; i++) {
		population[i].fitness = Fitness(population[i]);
		avgfit += population[i].fitness;
		if (population[i].fitness == 0) {
			return i + 1;
		}
	}
	return 0;
}

dl Parabola::MultInv() {
	dl sum = 0;
	for (ll i = 0; i < MAXPOP; i++) {
		sum += 1.0 / (population[i].fitness);
	}
	return sum;
}

void Parabola::GenerateLikelihoods() {
	dl multinv = MultInv(), last = 0;
	//cout << multinv << " -multinv\n";
	for (ll i = 0; i < MAXPOP; i++) {
		population[i].likelihood = last = last + ((1.0 / population[i].fitness / multinv) * 100.0);
	}
}

ll Parabola::GetIndex(dl val) {
	//cout << val << "\n";
	dl last = 0;
	for (ll i = 0; i < MAXPOP; i++) {
		//cout << population[i].likelihood << "\n";
		if (last <= val && val <= population[i].likelihood) return i;
		else last = population[i].likelihood;
	}
	return gen() % MAXPOP;
	//cout << "\n";
}

gene Parabola::Breed(ll p1, ll p2) {
	ll crossover = gen() % 2 + 1;// Create the crossover point (not first).
	ll first = gen() % 100;// Which parent comes first?

	gene child = population[p1];// Child is all first parent initially.

	ll initial = 0, final = 3;// The crossover boundaries.
	if (first < 50) initial = crossover;	// If first parent first. start from crossover.
	else final = crossover + 1;// Else end at crossover.

	for (ll i = initial; i < final; i++) {// Crossover!
		child.alleles[i] = population[p2].alleles[i];
		if (gen() % 101 < 5) child.alleles[i] = (dl)gen() / RAND_MAX / RAND_MAX * 5 - 10;
	}
	return child;// Return the kid...
}

void Parabola::CreateNewPopulation() {
	gene temppop[MAXPOP];
	for (ll i = 0; i < MAXPOP; i++) {
		ll parent1 = 0, parent2 = 0, iterations = 0;
		while (parent1 == parent2 || population[parent1] == population[parent2]) {
			parent1 = GetIndex((dl)gen() / RAND_MAX / RAND_MAX * 25);
			parent2 = GetIndex((dl)gen() / RAND_MAX / RAND_MAX * 25);
			iterations++;
			if (iterations > 100) {
				break;
			}
		}
		//cout << parent1 << " " << parent2 << "\n";
		temppop[i] = Breed(parent1, parent2);// Create a child.
	}
	for (int i = 0; i < MAXPOP; i++) population[i] = temppop[i];
}

int M_GRID_SIZE = 3, N_GRID_SIZE = 3, NEED_FIRST = 3, NEED_SECOND = 3;
inline std::vector<std::pair<int, int>> CheckDirections = { {1,1}, {1,0}, {0,1}, {1,-1} };

bool InGrid(int x, int y) {
	if (x >= 0 && y >= 0 && x < M_GRID_SIZE && y < N_GRID_SIZE) {
		return true;
	}
	return false;
}

int evaluatePosition(int x, int y, int player_, std::vector<int>& patterns, std::vector<std::vector<int>>& board_) {
	int ans = 0;
	for (auto [i, j] : CheckDirections) {
		bool g1 = true, g2 = true;
		int cnt1 = 1, cnt2 = 1;
		if (!InGrid(x + cnt1 * i, y + cnt1 * j)) {
			g1 = false;
		}
		while (InGrid(x + cnt1 * i, y + cnt1 * j) && board_[x + cnt1 * i][y + cnt1 * j] == player_) {
			cnt1++;
			if (!InGrid(x + cnt1 * i, y + cnt1 * j)) {
				g1 = false;
				break;
			}
		}
		if (!InGrid(x - cnt2 * i, y - cnt2 * j)) {
			g2 = false;
		}
		while (InGrid(x - cnt2 * i, y - cnt2 * j) && board_[x - cnt2 * i][y - cnt2 * j] == player_) {
			cnt2++;
			if (!InGrid(x - cnt2 * i, y - cnt2 * j)) {
				g2 = false;
				break;
			}
		}
		int free1 = 0, free2 = 0;
		while (InGrid(x + (cnt1 + free1) * i, y + (cnt1 + free1) * j) && board_[x + (cnt1 + free1) * i][y + (cnt1 + free1) * j] == -1) {
			free1++;
			if (!InGrid(x + (cnt1 + free1) * i, y + (cnt1 + free1) * j)) {
				g1 = false;
				break;
			}
		}
		while (InGrid(x - (cnt2 + free2) * i, y - (cnt2 + free2) * j) && board_[x - (cnt2 + free2) * i][y - (cnt2 + free2) * j] == -1) {
			free2++;
			if (!InGrid(x - (cnt2 + free2) * i, y - (cnt2 + free2) * j)) {
				g2 = false;
				break;
			}
		}

		bool gg1 = true, gg2 = true;
		int cntt1 = 1, cntt2 = 1;
		if (!InGrid(x + cntt1 * i, y + cntt1 * j)) {
			gg1 = false;
		}
		while (InGrid(x + cntt1 * i, y + cntt1 * j) && board_[x + cntt1 * i][y + cntt1 * j] == 1 - player_) {
			cntt1++;
			if (!InGrid(x + cntt1 * i, y + cntt1 * j)) {
				gg1 = false;
				break;
			}
		}
		if (!InGrid(x - cntt2 * i, y - cntt2 * j)) {
			gg2 = false;
		}
		while (InGrid(x - cntt2 * i, y - cntt2 * j) && board_[x - cntt2 * i][y - cntt2 * j] == 1 - player_) {
			cntt2++;
			if (!InGrid(x - cntt2 * i, y - cntt2 * j)) {
				gg2 = false;
				break;
			}
		}
		int freee1 = 0, freee2 = 0;
		while (InGrid(x + (cntt1 + freee1) * i, y + (cntt1 + freee1) * j) && board_[x + (cntt1 + freee1) * i][y + (cntt1 + freee1) * j] == -1) {
			freee1++;
			if (!InGrid(x + (cntt1 + freee1) * i, y + (cntt1 + freee1) * j)) {
				gg1 = false;
				break;
			}
		}
		while (InGrid(x - (cntt2 + freee2) * i, y - (cntt2 + freee2) * j) && board_[x - (cntt2 + freee2) * i][y - (cntt2 + freee2) * j] == -1) {
			freee2++;
			if (!InGrid(x - (cntt2 + freee2) * i, y - (cntt2 + freee2) * j)) {
				gg2 = false;
				break;
			}
		}
		if (cnt1 + cnt2 - 1 >= NEED_FIRST) {
			ans += patterns[0];
		}
		else if (cnt1 + cnt2 >= NEED_FIRST && free1 >= 1 && free2 >= 1) {
			ans += patterns[1];
		}
		else if (cnt1 + cnt2 + 1 >= NEED_FIRST && (free1 == 0 || free2 == 0)) {
			ans += patterns[4];
		}
		else if (cnt1 + cnt2 + 2 >= NEED_FIRST && free1 + free2 >= 3) {
			ans += patterns[5];
		}
		else {
			ans += patterns[6] * (cnt1 + cnt2 - 1) + patterns[7] * (free1 + free2);
		}

		if (cntt1 + cntt2 - 1 >= NEED_SECOND) {
			ans += patterns[2];
		}
		else if (cntt1 + cntt2 >= NEED_SECOND && freee1 >= 1 && freee2 >= 1) {
			ans += patterns[3];
		}
		else if (freee1 == 0 && gg1 || freee2 == 0 && gg2) {
			ans += patterns[7];
		}
	}
	return ans;
}



int main() {
	ll n;
	cin >> n;
	vector <ll> x(n), y(n);
	for (ll i = 0; i < n; i++) {
		cin >> x[i] >> y[i];
	}
	Parabola a(n, x, y);
	//cout << "-1\n10\n5";
	a.Solve();
	//cout << ans << "\n";
	for (ll i = 0; i < 3; i++) {
		cout << best.alleles[i] << "\n";
	}
	/*cout << "\n";
	best.alleles[0] = -1;
	best.alleles[1] = 10;
	best.alleles[2] = 5;
	cout << a.Fitness(best) << "\n";*/
    
    return 0;
}
