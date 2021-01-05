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
                if lang not in data:
                    data[lang] = {}
                if test not in data[lang]:
                    data[lang][test] = {"times": [], "mem":[], "filesize":0}
                stats = data[lang][test]
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

# order: test -> lang -> data
def extract_lang(data, lang):
    result = {}
    for key in data.keys():
        if lang in data[key]:
            result[key] = data[key][lang]
    return result

# order: lang -> test -> data
def get_all_tests(data):
    tests = []
    for lang in data:
        for test in data[lang]:
            if test in tests:
                pass
            else:
                tests.append(test)
    return tests

def collect_means(data, tests):
    result = {}
    for lang in data:
        lang_res = {}
        for test in tests:
            test_res = {}
            if test in data[lang]:
                test_res["times"] = np.mean(data[lang][test]["times"])
                test_res["mem"] = np.mean(data[lang][test]["mem"])
                test_res["filesize"] = data[lang][test]["filesize"]
            else:
                test_res = {"times" : 0, "mem" : 0, "filesize" : 0}
            lang_res[test] = test_res
        result[lang] = lang_res
    return result

def collect_deviations(data, tests):
    result = {}
    for lang in data:
        lang_res = {}
        for test in tests:
            test_res = {}
            if test in data[lang]:
                test_res["times"] = np.std(data[lang][test]["times"])
                test_res["mem"] = np.std(data[lang][test]["mem"])
            else:
                test_res = {"times" : 0, "mem" : 0}
            lang_res[test] = test_res
        result[lang] = lang_res
    return result

# Returns an updated list of just the tests the baseline was run on
# Assumes means already calculated, so each field is number rather than list
def normalise(means, deviations, baseline, keep=False):
    tests = []
    for test in means[baseline]:
        if means[baseline][test]["filesize"] != 0:
            tests.append(test)
    new_means = {}
    new_errs = {}
    for key in means:
        if key == baseline and (not keep):
            continue # implicitly 1 for every value, so don't plot
        new_means[key] = {}
        new_errs[key] = {}
        for test in tests:
            new_means[key][test] = {}
            new_errs[key][test] = {}
            mean_result = means[key][test]
            new_means[key][test]["times"] = mean_result["times"] / means[baseline][test]["times"]
            new_means[key][test]["mem"] = mean_result["mem"] / means[baseline][test]["mem"]
            new_means[key][test]["filesize"] = mean_result["filesize"] / means[baseline][test]["filesize"]
            err_result = deviations[key][test]
            new_errs[key][test]["times"] = err_result["times"] / means[baseline][test]["times"]
            new_errs[key][test]["mem"] = err_result["mem"] / means[baseline][test]["mem"]
    return tests, new_means, new_errs
        

#oldVersion = "FullCollection_08_12.txt" # TODO: Repeat with new benchmarks
# IR level optimisations
#opt1 = "cse_constants_dead_12_16_19_24.txt"
# Wasm level optimisations
#opt2 = "graph_opts_12_30.txt"

#data = readFile("opts_12_30_13_36.txt")
#data = readFile("passes_01_03_22_08.txt")
#data = readFile("pats_01_03_22_19.txt")

# scale all tests so that 1.0 = v2 performance in all metrics
#reference = "None"
#reference = "0" # For comparing number of passes to perform
#reference = "Off" # For comparing effect of regular vs optimised pattern matching
# True => keep reference column in results, useful if std plotted too
#tests, means, errors = normalise(means, errors, reference, True) 

# Will probably want plots for individual benchmarks too!
# Going to have issues with axis scale, need to split into fast/slow ones (Grain far slower than rest)
# https://stackoverflow.com/questions/14270391/python-matplotlib-multiple-bars
def bar_plot(ax, data, errors=None, colors=None, total_width=0.8, single_width=1, legend=True):
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

    errors: dictionary, optional
        Dictionary of standard deviations, corresponding structure to data

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
            if errors is None:
                bar = ax.bar(x + x_offset, y, width=bar_width * single_width, color=colors[i % len(colors)])
            else:
                err = errors[name][x]
                bar = ax.bar(x + x_offset, y, yerr=err, error_kw=dict(capsize=3),
                             width=bar_width * single_width, color=colors[i % len(colors)])

        # Add a handle to the last drawn bar, which we'll need for the legend
        bars.append(bar[0])

    # Draw legend if we need
    if legend:
        ax.legend(bars, data.keys())

## plot and display each metric in turn
def plotAll(means, errors, tests, baseline=False):
    for metric in ["times", "filesize", "mem"]:
        fig, ax = plt.subplots()
        metric_data = {key : [means[key][test][metric] for test in tests] for key in means}
        if metric == "filesize":
            err_data = None
        else:
            err_data = {key : [errors[key][test][metric] for test in tests] for key in errors}
        bar_plot(ax, metric_data, err_data, total_width=.8, single_width=.5)
        if baseline:
            ax.axhline(y=1.0, color='r', linestyle='dotted')
        plt.xticks(range(len(tests)), tests, rotation=90)
        plt.ylabel(metric)
        ax.set_title(metric)
        plt.tight_layout()
    plt.show()

# With using globals rather than putting locals in closure,
# is the optimiseGlobals optimisation actually useful?
# Answer: Yes, file size is lower in several cases
def displayOptimiseGlobals():
    data = readFile("compareoptsGlobals.txt")
    tests = get_all_tests(data)
    means = collect_means(data, tests)
    errors = collect_deviations(data, tests)
    reference = "None"
    tests, means, errors = normalise(means, errors, reference, True)
    plotAll(means, errors, tests)

# Globals only used for exports, all variables put in closures
def displayOptimiseLocals():
    data = readFile("compareoptsLocals.txt")
    tests = get_all_tests(data)
    means = collect_means(data, tests)
    errors = collect_deviations(data, tests)
    reference = "None"
    tests, means, errors = normalise(means, errors, reference, True)
    plotAll(means, errors, tests)

# Take the 'All' case from each of the above (include globalsOptimisation)
# Very slightly reduces memory usage in composition.ml
# Reduces filesize in a few cases, but only by max 5% and in some cases
# can be higher or much more variance (unclear if due to code or computer)
def compareLocalsGlobals():
    data1 = readFile("compareoptsGlobals.txt")
    data2 = readFile("compareoptsLocals.txt")
    data = {"NoneLocals" : data2["None"], "NoneGlobals" : data1["None"],
            "Locals" : data2["All"], "Globals" : data1["AllGlobal"]}
    tests = get_all_tests(data)
    means = collect_means(data, tests)
    errors = collect_deviations(data, tests)
    reference = "NoneLocals"
    tests, means, errors = normalise(means, errors, reference, True)
    plotAll(means, errors, tests)

compareLocalsGlobals()
