import pandas as pd
import matplotlib.pyplot as plt

# Read the CSV file into a pandas DataFrame
data = pd.read_csv('result.csv')

# Extract the 'Time', 'Ekin', and 'Epot' columns
time = data['Time']
ekin = data['Ekin']
epot = data['Epot']

# Plot the chart
# plt.plot(time, ekin, label='Ekin')
# plt.plot(time, epot, label='Epot')
# plt.xlabel('Time')
# plt.ylabel('Energy')
# plt.title('Time vs Energy')
# plt.legend()

# Display the chart
plt.show()
