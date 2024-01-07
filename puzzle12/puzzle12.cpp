#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#define BUFSIZE 1000

using namespace std;

// Function prototypes
vector<int> preprocess_sequence(const string& sequence);
vector<bool> find_hashes(const string& sequence);
unsigned long long int test_sequence(const string& sequence, const vector<int>& series);
unsigned long long int seq_recurse(const string& sequence, const vector<int>& pp_vect, const vector<bool>& h_vect, int index,
		const vector<int>& series, int series_index, vector<vector<unsigned long long int>>& cache_num, vector<vector<bool>>& cache_bool);

int main() {

	// Create a string to hold the line
	ifstream input_file ("puzzle12.txt");

	string buffer, sequence, numbers;
	size_t next_pos = 0;
	unsigned long long int result = 0, solution_a = 0, solution_b = 0;

	// A vector of string that stores the sequences
	vector<string> sequences;

	// A vector of vectors that stores the series lengths
	vector<vector<int>> series;

	// Input loop
	while(getline(input_file, buffer)) {
		// Extracting the sequence of records
		sequence = buffer.substr(0, buffer.find(' '));
		sequences.push_back(sequence);

		// Extracting the comma-separated numbers and adding them to the vector
		numbers = buffer.substr(buffer.find(' ') + 1, string::npos);
		vector<int> i_vect;

		while((next_pos = numbers.find(',')) != string::npos) {
			i_vect.push_back(stoi(numbers.substr(0, next_pos)));
			numbers = numbers.substr(next_pos + 1, string::npos);
		}

		i_vect.push_back(stoi(numbers));

		// Adding the vector of numbers to their overall container
		series.push_back(i_vect);
	}

	// Looping over all the sequences
	for(int i = 0; i < sequences.size(); i++) {

		result = test_sequence(sequences[i], series[i]);
		solution_a += result;
		
		// Now computing the solution for part B by duplicating the sequences
		string duplicated_seq = sequences[i] + string("?") +
			sequences[i] + string("?") +
			sequences[i] + string("?") +
			sequences[i] + string("?") +
			sequences[i];

		vector<int> duplicated_series(0);

		for(int j = 0; j < 5; j++) duplicated_series.insert(duplicated_series.end(), series[i].begin(), series[i].end());

		result = test_sequence(duplicated_seq, duplicated_series);
		solution_b += result;
	}

	cout << "Puzzle 12A: " << solution_a << endl;
	cout << "Puzzle 12B: " << solution_b << endl;

	return 0;
}

// A function that preprocesses a sequence to determine the maximum
// number of successive hashes that can fit at a given position
vector<int> preprocess_sequence(const string& sequence) {
	vector<int> output (sequence.size());

	for(int i = 0; i < sequence.size(); i++) {
		int j = i;

		while(sequence[j] != '.' && j < sequence.size()) {
			output[i]++;
			j++;
		}
	}

	return output;
}

// A function that returns a boolean vector with the positions of hashes
vector<bool> find_hashes(const string& sequence) {
	vector<bool> output (sequence.size() + 1);

	for(int i = 0; i < sequence.size(); i++) {
		output[i] = (sequence[i] == '#');
	}

	output[sequence.size()] = false;

	return output;
}

// A function that tests a sequence and series combination
unsigned long long int test_sequence(const string& sequence, const vector<int>& series) {

	// Initializing the return value to 0
	unsigned long long int return_value = 0;

	// We first preprocess the sequence to identify the streaks of #? and the positions of the hash symbols
	vector<int> pp_vect = preprocess_sequence(sequence);
	vector<bool> h_vect = find_hashes(sequence);

	// Initializing a cache of the results to avoid computing them every time
	// The cache is a matrix with as many rows as sequence elements
	// and as many columns as series elements
	// It stores the number of possibilities associated with a given
	// combination of sequence position and series position
	// We also need a boolean matrix indicating whether the value is cached
	vector<vector<unsigned long long int>> cache_num(sequence.size());
	vector<vector<bool>> cache_bool(sequence.size());

	for(int i = 0; i < cache_num.size(); i++) {
		cache_num[i].resize(series.size());
		cache_bool[i].resize(series.size());
	}

	// Then we launch the recursive function with the indices that fit for the first series element
	int i = 0;

	while(i < sequence.size()) {
		if(pp_vect[i] >= series[0] && !h_vect[i + series[0]]) return_value += seq_recurse(sequence, pp_vect, h_vect, i, series, 0, cache_num, cache_bool);
		if(h_vect[i]) break;
		i++;
	}

	return return_value;
}

// A recursive function that finds the number of matches for a starting index in the sequence and index in the series of integers
unsigned long long int seq_recurse(const string& sequence, const vector<int>& pp_vect, const vector<bool>& h_vect,
		int index, const vector<int>& series, int series_index, vector<vector<unsigned long long int>>& cache_num, vector<vector<bool>>& cache_bool) {

	// If the value is cached then we return it
	if(cache_bool[index][series_index]) return cache_num[index][series_index];

	// Initializing the return value
	unsigned long long int return_value = 0;

	// We store the values of the indices that the function was called with
	int initial_index = index;
	int initial_sindex = series_index;

	// If this is the last element of the sequence then we can increment the return value
	// if there are no remaining hashes
	if(series_index == (series.size() - 1)) {
		index += series[series_index];

		while(index < sequence.size()) {
			if(h_vect[index]) {
				// We update the cache
				cache_bool[initial_index][initial_sindex] = true;
				cache_num[initial_index][initial_sindex] = 0;
				return 0;
			}
			index++;
		}

		// If there were no hashes remaining then this is a match and we increment the return value
		return_value++;
	} else {
		// If this is not the last series element, then we need to loop over possible indices
		// for the next element in the sequence

		// But first we increment the index and the series_index
		index += series[series_index] + 1;
		series_index++;

		// We cut the computation short if the number of elements to fit is too large compared to the remaining sequence
		// We initialize to -1 because the last series element does not need a space in between
		int to_fit = -1;
		for(int i = series_index; i < series.size(); i++) to_fit += series[i] + 1;

		// We go forward with the next element in the series if:
		// - there is enough room to fit the series element
		// - the element after the selected sequence is not a #
		// This loop is performed as long as the remaining sequence can fit
		// We also cannot leave a hash behind because that would break the requirements
		while(index < sequence.size() && to_fit <= (sequence.size() - index)) {
			if(pp_vect[index] >= series[series_index] && !h_vect[index + series[series_index]])
				return_value += seq_recurse(sequence, pp_vect, h_vect, index, series, series_index, cache_num, cache_bool);
			if(h_vect[index]) break;
			index++;
		}
	}

	// We update the cache prior to returning the result
	cache_bool[initial_index][initial_sindex] = true;
	cache_num[initial_index][initial_sindex] = return_value;
	return return_value;
}

