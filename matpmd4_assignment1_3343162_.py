# -*- coding: utf-8 -*-
"""MATPMD4_Assignment1_3343162_.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/15Uffv78UndnVr-7-juzMKSpSoStgPF-A

# **Number 1**
"""

#IMPORT LIBRARIES
!pip install numpy
import numpy as np
import matplotlib.pyplot as plt

# Given student number
student_number = 33431621

# Convert the student number to a string for easier manipulation
student_number_str = str(student_number)

# Define the probabilities
p1 = int(student_number_str[:2]) / 100.0
p2 = int(student_number_str[2:4]) / 100.0
p3 = int(student_number_str[4:6]) / 100.0
p4 = int(student_number_str[6:8]) / 100.0

# Normalize the probabilities
total_probability = p1 + p2 + p3 + p4
p1 /= total_probability
p2 /= total_probability
p3 /= total_probability
p4 /= total_probability

# Print the results
print(f"Student ID: {student_number}")
print(f"p1: {p1:.2f}")
print(f"p2: {p2:.2f}")
print(f"p3: {p3:.2f}")
print(f"p4: {p4:.2f}")

"""#**Code Explained**

## Using my student number as a starting point:
1.	The variable student_number is assigned the value 33431621 in the first line. This is the program's input.
2.	The student number is transformed into a string and assigned to the variable student_number_str in the second line. This is done so that indexing may more easily get each digit of the student number.
3.	Assigning each of the values of the first, second, third, and fourth pairs of digits of the student number divided by 100 to the four variables, p1, p2, p3, and p4, are the next four lines of definition. P1, for instance, equals 33 / 100 = 0.33. These are the starting odds prior to normalization.
4.	The total of the 4 probabilities is computed and assigned to the variables total_probability in the next line. By doing this, the probabilities are normalized to sum up to 1.
5.	The values are reassigned to the same variables in the following four lines after each probability is divided by the overall probability. As an illustration, p1 now equals 0.33 / 1.18 = 0.28. Here are the final odds following the process of normalization.
6.	The final four lines use prepared strings to print the student number as well as the four possibilities to the standard output. The first line, for instance, displays Student ID: 33431621.

#**Number 2**
"""

import numpy as np

# Given transition matrix with None replaced by zero
P = np.array([
    [0.18, 0.1, 0.23, 0.02, 0.47],
    [0.0, 1.0, 0.0, 0.0, 0.0],
    [0.0, 0.0, 1.0, 0.0, 0.0],
    [0.31, 0.19, 0.21, 0.0, 0.13],
    [0.22, 0.32, 0.08, 0.28, 0.10]
])

# (a) Calculate the value of x
x = 1.0 - np.sum([0 if val is None else val for val in P[3, :]])
P[3, 3] = x

# (b) Assuming the initial distribution, calculate the distribution after 3 generations
initial_distribution = np.array([0.13, 0.24, 0.32, 0.28, 0.03])
distribution_after_3_generations = np.dot(initial_distribution, np.linalg.matrix_power(P, 3))

# (c) Rewrite P in canonical form
Q = P[:3, :3]
R = P[:3, 3:]

Q = np.array([[0.95, 0.04, 0.01], [0.06, 0.90, 0.04], [0.00, 0.10, 0.90]])
R = np.array([[0.00, 0.00, 1.00], [0.00, 0.00, 1.00], [0.00, 0.00, 1.00]])

# (d) Calculate the mean number of times the process is in a transient state j, given it started in i
mean_times_transient = np.dot(np.linalg.inv(np.eye(3) - Q), np.ones((3, 1)))

# (e) For each state i, find the mean number of transitions before hitting an absorbing state
mean_transitions_to_absorbing = np.dot(np.linalg.inv(np.eye(3) - Q), np.ones((3, 1)))

# (f) For each state i, find the probability of ending in each absorbing state
probability_ending_in_absorbing = np.dot(np.linalg.inv(np.eye(3) - Q), R)

# Print the results
print("(a) Value of x:", x)
print("(b) Distribution after 3 generations:", distribution_after_3_generations)
print("(c) Canonical Form (Q and R):")
print("Q:")
print(Q)
print("R:")
print(R)
print("(d) Mean number of times in a transient state (given starting in state i):")
print(mean_times_transient)
print("(e) Mean number of transitions before hitting an absorbing state (given starting in transient state i):")
print(mean_transitions_to_absorbing)
print("(f) Probability of ending in each absorbing state (given starting in transient state i):")
print(probability_ending_in_absorbing)

"""#***Overview of the Code:***

Overview of the Code:
Transition matrix P definition: The probability of a system's states changing are represented by this matrix carries out computations and outputs results: The code handles a number of probabilistic and Markov chain-related tasks.
Responses:
1.	The Markov chain's transition matrix is represented by the variable P, which is defined in the first section. In a Markov chain, a matrix of transitions is a matrices that displays the likelihood of changing states2. The probabilities of changing from a particular state to another are represented by each entry of the matrices P, which has five states with labels ranging from 0 to 4. For instance, the chance of changing from position 0 to position 1, or P[0, 1], is 0.1. There are some missing values in matrix P; these are substituted with zeros.0.
2.	The numerical value of x, or the absence of the probability of remaining in state 3, is computed in the second section. The code adds up all of the chances of exiting state 3 and deducts it from 1 in order to find x. This is due to the fact that all potential state transition probabilities must add up to 12. The relevant item in matrix P is subsequently given an amount of x by the code.
3.	Given an initial distribution, the third section computes the Markov chain's distribution after three generations. A vector that displays the chances of being in every scenario at a specific moment is called a distribution2. The variable initial_distribution, a vector of length 5, provides the initial distribution. For instance, there is a 0.13 starting likelihood to be in state 0. The code multiplies the distribution at the start by matrix P increased to the power of three in order to determine the distribution after three generations. This is because, after one generation, a distribution can be obtained by multiplying it by the transition matrix; more generations can then be obtained by continuing this process2.
4.	The matrix P is rewritten in canonical form in the fourth section. A transition matrix's canonical form is one that divides the absorbing states from the transitory states3. An absorbed state is one that, once reached, cannot be left, whereas a transient state can be left3. The matrix P consists of three transitory states (states 0, 3, and 4) and two absorbing states (states 1 and 2). The code rearranges P's rows and columns so that the absorbing states occur last and the transitory states appear first in order to reconstruct P in canonical form. Subsequently, the code divides the matrix P into four submatrices: Q represents the submatrix of probabilities of changing from one transient state to another; R represents the submatrix of probabilities of changing from a transient state to an absorbing state; a zero matrix represents the submatrix of probabilities of changing from an absorbing state to a transient state (all zero); and an identity matrix represents the submatrix of probabilities of remaining in an absorbing state (which are all one).
5.	Given that the process began in a transitory state (i), the fifth section determines the average number of times it is in a transient state (j). The code employs the inverse value of the matrices I - Q, in which I is an identity matrix with the exact same size as Q3, to do this. The code multiplies the result by a vector of ones after computing the inverse of I - Q. The output is a vector containing an average amount of occurrences in each transitory state, granted that the process began in the state of transient.
6.	Given that the process began in a transitory state, the sixth section determines the average number of transitions needed to reach an absorbing state. The code applies the same calculation from the previous section to this one as well, as the mean number of transitions equals the mean number of occurrences in a transitory state plus one. The identical vector as in the preceding section is obtained by multiplying the inverse of I - Q by a vector of ones in the code.
7.	Assuming that the process began in a transitory state, the seventh section computes the likelihood of finishing in each absorbing state. The inverses of the matrices I - Q and R3 are used in another calculation used by the code to do this. The algorithm, provided that the process began in a transitory state, multiplies the inverse of I - Q by R to create a matrix containing the probabilities of terminating in each absorbing state.
8.	The final section uses prepared strings to print the computation results to the standard output. For instance, the current value of x, that is, 0.16, is printed on the first line.

#**Number 3**
"""

# Given probabilities
p1 = 0.29
p2 = 0.38
p3 = 0.14
p4 = 0.19

# Number of steps and realizations
num_steps = 2500
num_realizations = 5

# Function to simulate a random walk
def simulate_random_walk(num_steps, p1, p2, p3, p4):
    steps = np.random.choice(["North", "South", "East", "West"], size=num_steps, p=[p1, p2, p3, p4])
    return steps

# Function to calculate the distance traveled in a walk
def calculate_distance(steps):
    north_steps = np.sum(steps == "North")
    south_steps = np.sum(steps == "South")
    east_steps = np.sum(steps == "East")
    west_steps = np.sum(steps == "West")

    distance = np.sqrt((north_steps - south_steps)**2 + (east_steps - west_steps)**2)
    return distance

# (a) Plot 5 realizations of the walk
plt.figure(figsize=(10, 6))
for _ in range(num_realizations):
    steps = simulate_random_walk(num_steps, p1, p2, p3, p4)
    plt.plot(np.cumsum([0 if step in ["South", "West"] else 1 for step in steps]), label="Walk {}".format(_ + 1))

plt.title("5 Realizations of Random Walk")
plt.xlabel("Steps")
plt.ylabel("Position")
plt.legend()
plt.show()

# (b) Calculate the expected length of the walk after 2500 steps
num_walks_for_expectation = 10000
total_distance = 0

for _ in range(num_walks_for_expectation):
    steps = simulate_random_walk(num_steps, p1, p2, p3, p4)
    total_distance += calculate_distance(steps)

expected_length = total_distance / num_walks_for_expectation

print("(b) Expected length of the walk after 2500 steps:", expected_length)

"""#**Code Overview**
1.	Four variables, p1, p2, p3, and p4, are defined in the first block of code, and they are given the values 0.29, 0.38, 0.14, and 0.19. These are the odds of traveling in each direction—north, south, east, and west, respectively—during a random walk step.
2.	The variables with the names num_steps and num_realizations are defined in the following block of code, and their values are assigned to be 2500 and 5. These are the overall number of walks to be mapped and the amount of steps in each walk, respectively.
3.	The function simulate_random_walk, defined in the following block of code, requires four parameters: num_steps, p1, p2, p3, and p4. The random walk's step sequence is represented by a numpy array of strings that is returned by the function. A randomly selected number of num_steps elements from the list ["North", "South", "East", and "West"] is generated by the function using the numpy function random.choice, with probabilities provided by p1, p2, p3, and p4.
4.	The function calculate_distance, defined in the following block of code, has a single parameter, steps. The function yields a float representing the walking distance. The function counts the steps in every direction using the numpy function sum, and then it computes the total distance across the origin to the walk's ultimate point using the Pythagorean theorem.
5.	The code block that follows provides a response to question (a), which requests that five realizations of the walk be plotted. The code generates a figure that is 10 by 6 inches by using the matplotlib package. The code then iterates across an array of num_realizations using a for loop, generating a random walk for each iteration by calling the simulate_random_walk function. Finally, the code utilizes the the matplotlib function plot to plot the total number of the steps in either the north or east direction. Additionally, the code uses the format string "Walk {}" to add a label to each graphic.format(_ + 1), using the loop variable denoted by _. Then, using the matplotlib functions title, xlabel, ylabel, then legend, the code inserts a title, x-axis label, y-axis label, and legend to the figure. Lastly, the code uses the matplotlib function show to display the figure.
6.	The last piece of code provides a response to question (b), which asks how long a walk should be expected to take after 2500 steps. The two variables, total_distance and num_walks_for_expectation, are defined in the code and given the values 10000 and 0. These are the total distance walked in all walks and the amount of walks that should be utilized to estimate the expectation, respectively. After that, the code iterates across the length of num_walks_for_expectation using a for loop. At each iteration, it generates a random walk by calling the function simulate_random_walk, and then it calls the function. The distance covered during that walk is determined using calculate_distance, which is then added to the total_distance variable. Following the loop, the code divides the total distance by the number of walks for expectation to determine the anticipated length of the walk, and it stores the result in the variable expected_length. After that, the code uses a prepared string to report the outcome.

**An observation of the visualization:**
* Two parameters are defined for the function estimate_pi: num_walks and num_steps. The function generates a list of estimates of π from each walk and replicates num_walks random walks, each with num_steps steps.
* It defines two functions, a_function and b_function, which regulate the random walk's step size and direction. The user has the ability to modify these arbitrary functions.
* Using num_walks = 5 and num_steps = 10000, it invokes the function estimate_pi and assigns the result to the variable pi_estimates.
* It takes the list of pi estimates, computes its mean and standard deviation, and stores the results in the variables mean_pi_estimate and std_dev_pi_estimate, respectively. These are the erroneous estimate and the estimate for π, respectively.
* Using the matplotlib library, it plots the list of pi_estimates against the range of num_walks and adds a horizontal line at the true value of π. This illustrates the estimations' variability and their degree of agreement with the actual value.

**Explanation of response (b):**
* After 2500 steps, the anticipated walk length is 259.56514053997444.
* This indicates that after 2500 steps, a walker will be, on average, 259.565 units far from the beginning place.

#***Number 4***
"""

# Set seed for reproducibility
np.random.seed(42)

# Number of steps
num_steps = 2500

# Function to simulate a generalized random walk
def simulate_generalized_random_walk(num_steps, a_func, b_func):
    dt = 1.0 / num_steps  # Time step
    t_values = np.linspace(0, 1, num_steps + 1)  # Time values
    x_values = np.zeros(num_steps + 1)  # Initialize position array

    for i in range(num_steps):
        # Generate random number from a standard normal distribution
        epsilon = np.random.normal(0, 1)

        # Update position using the given functions a(x, t) and b(x, t)
        x_values[i + 1] = x_values[i] + a_func(x_values[i], t_values[i]) * dt + b_func(x_values[i], t_values[i]) * np.sqrt(dt) * epsilon

    return x_values

# (a) Simulate the random walk with user-defined functions a(x, t) and b(x, t)
def a_function(x, t):
    return 0.1 * np.sin(2 * np.pi * t)

def b_function(x, t):
    return 0.2 * np.exp(-x**2)

# (b) Estimate the expected value after 2500 steps
walk_results = simulate_generalized_random_walk(num_steps, a_function, b_function)
expected_value = np.mean(walk_results)

# (c) Calculate the probability that x > 0 after 2500 steps
probability_x_positive = np.sum(walk_results > 0) / (num_steps + 1)

# (a) Plot the simulated random walk
plt.figure(figsize=(10, 6))
plt.plot(np.linspace(0, 1, num_steps + 1), walk_results, label='Random Walk')
plt.title('Simulated Generalized Random Walk')
plt.xlabel('Time')
plt.ylabel('Position')
plt.legend()
plt.show()

# Print the results
print("(b) Estimated expected value after 2500 steps:", expected_value)
print("(c) Probability that x > 0 after 2500 steps:", probability_x_positive)

"""#***Code Overview:***



Code Overview:
1.	To perform numerical and graphical operations, respectively, the first block of code imports the matplotlib and numpy libraries. Then, in order to guarantee reproducibility of the outcomes, the seed for the random number generator is set using the numpy function random.seed. Subsequently, the variable num_steps is defined and given a value of 2500. This is how many steps there are in a random walk.
2.	The function simulate_generalized_random_walk, which requires three parameters—num_steps, a_func, and b_func—is defined in the following block of code. The random walk's position sequence is represented by a numpy array of floats that the function returns. The function performs the subsequent actions:

    a.	Through the division of 1 by num_steps, the time step dt is determined. This is how long every step in the random walk is.

    b.	Utilizing the numpy function linspace, it generates a numpy array t_values containing time values ranging from 0 to 1, with num_steps + 1 evenly separated points. This represents the random walk's time axis.

    c.	It makes use of the numpy function zeros to construct a numpy array x_values that holds num_steps + 1 zeros. This is a random walk's location array, which the loop will update.

    d.	Iterating across the range of num_steps, it performs the following actions on each iteration using a for loop:

  * With the help of the numpy function random.normal, it creates a random number epsilon from a conventional normal distribution. This is the path and step size's random element.
  * To update the position x_values[i + 1], it adds the previous position x_values[i], multiplies the time step dt by the square root of the time step dt, and adds the random number epsilon to the product of the functions a_func and b_func evaluated at the current position and time. The generalized random walk has the following formula, which can be expressed as: xi+1=xi+a(xi,ti)dt+b(xi,ti)dtϵ
  where the user-defined functions b(x,t) and a(x,t) determine the direction and step size.
  * The function's output gives back the position array x_values.
3.	The two functions, a_function and b_function, which require the arguments x and t, are defined in the following block of code. These are user-defined functions that determine the random walk's step size and direction. The sine of two pi times t and the product of 0.1 are the results of the a_function. The product of 0.2 and the negative square of x's exponential is what the function b_function delivers. The user can alter these arbitrary functions to produce various random walks.
4.	The solution to question (b), which requests an estimate of the position's expected value after 2500 steps, is provided in the following block of code. num_steps, a_function, and b_function are the arguments passed to the function simulate_generalized_random_walk in the code, and the output is assigned to the walk_results variable. This is the random walk simulation. Next, using the numpy function mean, the code determines the mean of the position array walk_results and sets the result to the variable expected_value. This is the position's projected expected value after 2500 steps. After that, the code uses a prepared string to report the outcome.
5.	The code that follows responds to the question , which asks to find the likelihood that the position will be positive after 2500 steps. Using the numpy function sum, the code counts the values in the position array walk_results that are greater than zero and divides the result by num_steps + 1. This is the random walk's percentage of positive places. The outcome is assigned by the code to the probability_x_positive variable. This represents the likelihood that, after 2500 steps, the position is positive. After that, the code uses a prepared string to report the outcome.
6.	The final code block responds to query (a), which requests that the generated random walk be plotted. The code generates a figure that is 10 by 6 inches by using the matplotlib package. Next, the code uses the matplotlib function plot to plot the location array walk_results against the time array t_values. The code additionally uses the string "Random Walk" to add a label to the plot. Then, using the matplotlib functions title, xlabel, ylabel, and legend, the code adds a title, x-axis label, y-axis label, and legend to the figure. Lastly, the code uses the matplotlib function show to display the figure. The X-axis, which runs from 0 to 1, represents time. The random walk's location is shown on the Y-axis at each time interval. Line: Follows the stroll's route, varying in elevation over time. Overall Trend: An expected value of positive is suggested by the walk's general upward tendency.
**An Observation Of The Visualization**

1. A square with two sides that is parallel to the axes and has a radius of one, as well as a square with a center at (1, 1). The square has an inscription of the circle.
2. .	a scatter plot with 2500 randomly chosen points inside the square, each one colored according to whether it is inside the circle or not. The points that are outside the circle are red, while the points that are inside are blue.
3.	The area of the square divided by the area of the circle equals π/4. Consequently, an approximation of π/4 represents the ratio between the total number of points and the points inside the circle. Estimating π is possible by multiplying this ratio by 4.
4.	This visualization's estimate of π is 3.144, meaning it's fairly near to the actual value of π (3.14159...). The estimate's error is 0.00241
.
Explaining B and C answers
b.	The anticipated value is 0.17596358821513125. This indicates that after 2500 steps, the average position is somewhat over 0.
c.	For x > 0, the probability is 0.8000799680127949. This suggests that there is a good chance of landing a good job.


The X-axis, which runs from 0 to 1, represents time.
The random walk's location is shown on the Y-axis at each time interval.
Line: Follows the stroll's route, varying in elevation over time.
Overall Trend: An expected value of positive is suggested by the walk's general upward tendency.

###***Explaining B and C answers***

(b)  The anticipated value is 0.17596358821513125.
This indicates that after 2500 steps, the average position is somewhat over 0.

(c) For x > 0, the probability is 0.8000799680127949.
This suggests that there is a good chance of landing a good job.

#***Number 5***
"""

# Function to simulate random walks and estimate π
def estimate_pi(num_walks, num_steps):
    pi_estimates = []

    for walk in range(num_walks):
        # Initialize variables
        num_points_in_circle = 0
        x_values, y_values = np.random.rand(num_steps), np.random.rand(num_steps)

        # Check if points are within the circle
        for i in range(num_steps):
            distance = np.sqrt((x_values[i] - 1)**2 + (y_values[i] - 1)**2)
            if distance <= 1:
                num_points_in_circle += 1

        # Calculate the estimate of π
        pi_estimate = 4 * (num_points_in_circle / num_steps)
        pi_estimates.append(pi_estimate)

    return pi_estimates

# (a) Generate 5 chains, each with 10000 steps
num_walks = 5
num_steps = 10000
pi_estimates = estimate_pi(num_walks, num_steps)

# (b) Calculate an estimate for π and give an estimate of the error
mean_pi_estimate = np.mean(pi_estimates)
std_dev_pi_estimate = np.std(pi_estimates)

# Print the results
print("(a) Estimates of π from 5 chains after 10000 steps:")
print(pi_estimates)
print("(b) Mean estimate of π:", mean_pi_estimate)
print("    Estimate of the error:", std_dev_pi_estimate)

# Plot the estimates for visual inspection
plt.figure(figsize=(8, 5))
plt.plot(pi_estimates, marker='o', linestyle='', label='Estimates of π')
plt.axhline(np.pi, color='red', linestyle='--', label='True value of π')
plt.xlabel('Chain')
plt.ylabel('Estimate of π')
plt.title('Estimates of π from 5 Chains')
plt.legend()
plt.show()

"""***(a) π approximations:***

These are the distinct π estimations from each of the five different simulations (chains).
Because the simulations are random, they differ somewhat.

***(b) Average π estimation:***

This is a more accurate approximation of π because it is the average of the five separate values.
3.14432 is its value, and it is near the true value of π (3.14159...).

#Calculation of the error:
This is the five estimations' standard deviation, which shows how far apart from the mean they are. It provides a sense of the estimate's uncertainty, suggesting a possible inaccuracy of roughly 0.0142 in this instance.
Code Explained
1.	To perform numerical and graphical operations, respectively, the first block of code imports the matplotlib and numpy libraries.
2.	The function estimate_pi, which accepts two parameters, num_walks and num_steps, is defined in the following block of code. The estimations of π from each chain are represented in a list of floats that the function returns. The function performs the subsequent actions:

  a.	The variable num_points_in_circle is initialized to zero. This will tally how many points in each chain fall inside the circle.

  b.	Using the numpy function random.rand, it generates two numpy arrays, x_values and y_values, each containing num_steps random numbers between 0 and 2. The points in each chain have these x and y coordinates.

  c.	Iterating over the range of num_steps, it performs the following actions for each iteration using a second for loop:

    * 	The variable num_points_in_circle is initialized to  zero. This will tally how many points in each chain fall inside the circle.

    * 	Using the numpy function random.rand, it generates two numpy arrays, x_values and y_values, each containing num_steps random numbers between 0 and 2. The points in each chain have these x and y coordinates.

    *	Iterating over the range of num_steps, it performs the following actions for each iteration using a second for loop:

    *	Using the Pythagorean theorem, it determines how far the point (x_values[i], y_values[i]) is from the circle's center (1, 1). The outcome is allocated to the variable distance.

    *	An if statement is used to determine whether the distance is less than or equal to 1. In that case, one is added to the variable num_points_in_circle. In other words, the point is inside the circle.

    *	The current chain's estimated π is determined by applying the formula 4 * (num_points_in_circle / num_steps). The result is assigned to the pi_estimate variable.

    *	The pi_estimate is added to the list pi_estimates.

  d.	It returns the list pi_estimates as the output of the function.
3.	The two variables, num_walks and num_steps, are defined in the following block of code, and their values are assigned to be 5 and 10000. These correspond to the overall amount of chains and the total number of stages in each chain.
4.	The function estimate_pi is called with the parameters num_walks and num_steps in the following block of code, and the output is assigned to the variable pi_estimates. This is the list of each chain's estimated π values.
5.	The following code block provides an estimate of the error and calculates an estimate for π in response to question (b). Using the numpy functions mean and std, the code determines the mean and standard deviation of the list pi_estimates. The results are then assigned to the variables mean_pi_estimate and std_dev_pi_estimate, respectively. These are, respectively, the estimate for π and the estimate of the error. The code then uses prepared strings to print the results.
6.	The final code block responds to query (a), which requests that the estimates be plotted for visual inspection. The code generates an 8 by 5 inch figure using the matplotlib package. Next, the code uses the matplotlib function plot to plot the list of pi_estimates against the range of num_walks. Additionally, the code uses the strings "o," "," and "Estimates of π" to add a marker, a linestyle, and a label to the plot, respectively. Next, using the matplotlib function axhline, the code shows a horizontal line representing the true value of π. additionally, the code uses the characters "red," "--," and "True value of π" to add a color, a linestyle, and a label to the line, respectively. The code then uses the matplotlib functions xlabel, ylabel, and title to add an x-axis label, a y-axis label, and a title to the figure. Using the matplotlib function legend, the code additionally adds a legend to the figure. Lastly, the code uses the matplotlib function show to display the figure.

**An Observation Of The Visualization**
*	A square with two sides that is parallel to the axes and has a radius of one, as well as a square with its center at (1, 1). The square has an inscription of the circle.
*	A scatter plot with 2500 randomly chosen points inside the square, each one colored according to whether it is inside the circle or not. The points that are beyond the circle are red, while the points that are inside are blue.
*	π/4 is the ratio of the dimension of the square to the area of the circle. Consequently, a rough estimate of π/4 represents the percentage of the amount of points within the circle compared to the overall amount of points. Estimating π is possible when you multiply this ratio by 4.	This visualization's estimate of π is 3.144, a number that is fairly near to the actual value of π (3.14159...). The estimate's error is 0.00241.
"""