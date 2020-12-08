import numpy as np
import matplotlib.pyplot as plt

def readFile(filename):
    with open(filename) as f:
        data = {}
        for line in f:
            line = line.strip()
            if len(line.split(" ")) < 2:
                lang = line
            else:
                line = line.split(" ")
                test = line[0]
                if test not in data:
                    data[test] = {}
                if lang not in  data[test]:
                    data[test][lang] = {"times": [], "mem":[], "filesize":-1}
                stats = data[test][lang]
                if lang == "Grain":
                    if line[1] == "time":
                        stats["times"].append(float(line[2]))
                    else:
                        stats["mem"] = [int(line[2])]
                        stats["filesize"] = int(line[3])
                else:
                # TODO: Check order of mem vs time for ML/JS/C
                    stats["times"].append(float(line[1]))
                    stats["mem"].append(int(line[2]))
                    stats["filesize"] = int(line[3])
        return data

# TODO: Gather variance statistics to work out number of repeats needed (Normalised variance more useful?)
data = readFile("ImprovedGrainTests.txt")
langs = ["C", "Grain", "JS", "OCaml"]
benchmarks = ["alltrees", "arith", "composition", "funcrec"]
means = {lang : {} for lang in langs}
stds = {lang : {} for lang in langs}
for bench in data:
    for lang in data[bench]:
        times = data[bench][lang]["times"]
        #print(bench, lang, np.mean(times), np.std(times))
       # print(bench, lang, np.std(times)/np.mean(times))
      #  print(np.std(times)/np.mean(times))
        means[lang][bench] = np.mean(times)
        stds[lang][bench] = np.std(times)

# Due to test not being present
means["C"]["composition"] = 0

# Will probably want plots for individual benchmarks too!
# Going to have issues with axis scale, need to split into fast/slow ones (Grain far slower than rest)
# https://stackoverflow.com/questions/14270391/python-matplotlib-multiple-bars
def bar_plot(ax, data, colors=None, total_width=0.8, single_width=1, legend=True):
    """Draws a bar plot with multiple bars per data point.

    Parameters
    ----------
    ax : matplotlib.pyplot.axis
        The axis we want to draw our plot on.

    data: dictionary
        A dictionary containing the data we want to plot. Keys are the names of the
        data, the items is a list of the values.

        Example:
        data = {
            "x":[1,2,3],
            "y":[1,2,3],
            "z":[1,2,3],
        }

    colors : array-like, optional
        A list of colors which are used for the bars. If None, the colors
        will be the standard matplotlib color cyle. (default: None)

    total_width : float, optional, default: 0.8
        The width of a bar group. 0.8 means that 80% of the x-axis is covered
        by bars and 20% will be spaces between the bars.

    single_width: float, optional, default: 1
        The relative width of a single bar within a group. 1 means the bars
        will touch eachother within a group, values less than 1 will make
        these bars thinner.

    legend: bool, optional, default: True
        If this is set to true, a legend will be added to the axis.
    """

    # Check if colors where provided, otherwhise use the default color cycle
    if colors is None:
        colors = plt.rcParams['axes.prop_cycle'].by_key()['color']

    # Number of bars per group
    n_bars = len(data)

    # The width of a single bar
    bar_width = total_width / n_bars

    # List containing handles for the drawn bars, used for the legend
    bars = []

    # Iterate over all data
    for i, (name, values) in enumerate(data.items()):
        # The offset in x direction of that bar
        x_offset = (i - n_bars / 2) * bar_width + bar_width / 2

        # Draw a bar for every value of that type
        for x, y in enumerate(values):
            bar = ax.bar(x + x_offset, y, width=bar_width * single_width, color=colors[i % len(colors)])

        # Add a handle to the last drawn bar, which we'll need for the legend
        bars.append(bar[0])

    # Draw legend if we need
    if legend:
        ax.legend(bars, data.keys())

toplot = {lang : [means[lang][b] for b in benchmarks] for lang in means}
#langsnotgrain = ["C", "JS", "OCaml"]
#benchnotarith = ["alltrees", "composition", "funcrec"]
#toplot = {lang : [means[lang][b] for b in benchnotarith] for lang in langs}

fig, ax = plt.subplots()
bar_plot(ax, toplot, total_width=.8, single_width=.9)
plt.xticks(range(len(benchmarks)), benchmarks)
plt.ylabel("time (ms)")
plt.show()
