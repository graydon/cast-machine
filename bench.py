#############
## Testing ##
#############

import pickle
import sys

print(sys.argv[1])

if (sys.argv[1] != "--plot"):
    # import sys
    import os
    import glob
    import time
    import timeit

    tests = glob.glob("bench/**")
    tests.remove("bench/data")
    tests.remove("bench/artifact")
    tests.remove("bench/morse-code")
    tests.remove("bench/dfs")
    tests.remove("bench/polyadhoc")
    tests.append("bench/polyadhoc")
    print("Pending tests:", tests)

    time, runtime, bench = [], [], []
    # bench = 100

    def get_base (s):
        return os.path.basename(s)

    def add_data(t, r, b):
        time.append(t)
        runtime.append(r)
        bench.append(b)

    data = []
    args = ["1000", "", "", ""] # arguments provided for each benchmark (in order)
    n_bench = 20

    for (i,bench_dir) in enumerate(tests):
        typed_bench = bench_dir + "/typed/main.gcl"
        if bench_dir == "bench/sieve":
            typed_bench2 = bench_dir + "/typed/main2.gcl"
            typed_bench3 = bench_dir + "/typed/main3.gcl"
        untyped_bench = bench_dir + "/untyped/main.gcl"

        get_cmd = lambda runtime: lambda test : ('os.system("' + runtime + test + " " + args[i] + '")')
        get_cmd_machine = get_cmd("./main.native --load ")
        get_cmd_machine_sym = \
            lambda test: 'os.system("./main.native --load '+ test + " " + args[i] + ' --symbolic")'
        get_cmd_machine_sym_cap = \
            lambda test: 'os.system("./main.native --load '+ test + " " + args[i] + ' --symbolic-cap")'

        def run_bench (runtime, test, get_cmd=get_cmd_machine):
            print("Starting test " + test)
            print("Runtime:" + runtime)
            t = timeit.timeit(get_cmd(test),setup='import os',number=n_bench)
            # t2 = time.process_time()
            print("Program " + test + " executed in time: {:0.2e}".format(t))
            print(get_base(bench_dir))
            add_data(t / n_bench, runtime, get_base(bench_dir))

        # Machine tests
        run_bench("Typed", typed_bench)
        if bench_dir == "bench/sieve": # first bench = sieve
            run_bench("Typed2", typed_bench2)
            run_bench("Typed3", typed_bench3)
        run_bench("Symbolic", typed_bench, get_cmd=get_cmd_machine_sym)
        if bench_dir == "bench/sieve":
            run_bench("Symbolic2", typed_bench2, get_cmd=get_cmd_machine_sym)
            run_bench("Symbolic3", typed_bench3, get_cmd=get_cmd_machine_sym)
        run_bench("SymbolicCap", typed_bench, get_cmd=get_cmd_machine_sym_cap)
        if bench_dir == "bench/sieve":
            run_bench("SymbolicCap2", typed_bench2, get_cmd=get_cmd_machine_sym_cap)
            run_bench("SymbolicCap3", typed_bench3, get_cmd=get_cmd_machine_sym_cap)
        run_bench( "Untyped", untyped_bench)

        # Python tests
        get_cmd_python = get_cmd("python ")
        # print(get_cmd_python("ah"))
        test_python = bench_dir + "/python/main.py "
        run_bench("Python", test_python, get_cmd=get_cmd_python)

        # Ocaml tests
        get_cmd_ocaml = get_cmd("./")
        test_ocaml = bench_dir + "/ocaml/main.exe "
        os.system("ocamlc -o " + test_ocaml + " " + bench_dir + "/ocaml/main.ml")
        run_bench("OCaml", test_ocaml, get_cmd=get_cmd_ocaml)

    d = {
        'time' : time,
        'runtime' : runtime,
        'bench' : bench
    }

    with open("bench/data/" + sys.argv[1] + ".dat","wb") as fo:
        pickle.dump(d, fo)


##############
## Plotting ##
##############

import numpy as np
import matplotlib.pyplot as plt
# matplotlib.style.use('ggplot')
import seaborn as sns
import pandas as pd


# sns.set(style="whitegrid")
rs = np.random.RandomState(8)


# d = {
#     'time': [2, 45, 150, 150, 200] * 2, 
#     'runtime': ["OCaml", "Untyped", "Typed", "Typed Symbolic", "Python"] * 2,
#     # 'runtime': ["other", "machine", "machine", "machine", "other"],
#     'bench': ["sieve","sieve","sieve","sieve","sieve"] + (["crypto"] * 5)
# }
# fig, axs =plt.subplots(2,1)

with open("bench/data/" + sys.argv[2] + ".dat","rb") as fo:
    d = pickle.load(fo)

print(d)

bench_data = pd.DataFrame(data=d)

all_poly = bench_data[bench_data.bench == "polyadhoc"]

print(all_poly)

bench_data = bench_data[bench_data.bench != "polyadhoc"]

bench_data = bench_data.append(all_poly)

collabel=("OCaml", "Untyped", "Typed", "Python")
# axs[0].axis('tight')
# axs[0].axis('off')
print(bench_data.time)
print(bench_data)
cell_text = [list(round(x,4) for x in bench_data['time'])]
print(cell_text)
# axs[0].table(cellText=bench_data.time,colLabels=collabel,rowLabels=("runtime","sieve"))
# axs[0].table()

# bench_data.groupby(['bench','runtime','time']).size().unstack().plot(kind='bar',stacked=True)
# summary=pd.DataFrame(bench_data['time'].describe())
# bench_data.plot(x="bench", y="time", kind="bar", table=True,ax=axs[0])

# fig, ax = plt.subplots(1, 1, figsize = (40, 22), dpi=250)
# ax = fig.add_subplot(111)
g = sns.catplot(x="bench", y="time", hue="runtime", data=bench_data, 
    height=6, kind="bar", palette="rocket")
# ax.set_xlabel('')
g.despine(left=True)
# ax2 = fig.add_subplot(222)
# plt.table(cellText=cell_text,
#     rowLabels=["sieve"],
#     colLabels=bench_data.runtime,
#     loc='bottom')
# plt.subplots_adjust(left=0.2, bottom=0.2)
# plt.xlabel
# ax.get_xaxis().set_visible(False)

# No Ocaml or Python
machine_data = bench_data[bench_data.runtime != 'OCaml'][bench_data.runtime != 'Python']

# fig, ax = plt.subplots(1, 1, figsize = (40, 22), dpi=250)
# ax = fig.add_subplot(111)
g = sns.catplot(x="bench", y="time", hue="runtime", data=machine_data, 
    height=6, kind="bar", palette="rocket")
# ax.set_xlabel('')
g.despine(left=True)
# ax2 = fig.add_subplot(222)
# plt.table(cellText=cell_text,
#     rowLabels=["sieve"],
#     colLabels=bench_data.runtime,
#     loc='bottom')
# plt.subplots_adjust(left=0.2, bottom=0.2)



# No Ocaml or Python
nosym_data = bench_data[bench_data.runtime != 'OCaml'] \
    [bench_data.runtime != 'Python'][bench_data.runtime != 'Symbolic'] \
        [bench_data.runtime != 'Symbolic2'][bench_data.runtime != 'Symbolic3'] \
            [bench_data.runtime != 'SymbolicCap'] \
        [bench_data.runtime != 'SymbolicCap2'][bench_data.runtime != 'SymbolicCap3']

# fig, ax = plt.subplots(1, 1, figsize = (40, 22), dpi=250)
# ax = fig.add_subplot(111)
g = sns.catplot(x="bench", y="time", hue="runtime", data=nosym_data, 
    height=6, kind="bar", palette="rocket")
# ax.set_xlabel('')
g.despine(left=True)
# ax2 = fig.add_subplot(222)
# plt.table(cellText=cell_text,
#     rowLabels=["sieve"],
#     colLabels=bench_data.runtime,
#     loc='bottom')
# plt.subplots_adjust(left=0.2, bottom=0.2)

nosym_data = bench_data[bench_data.runtime != 'OCaml'] \
    [bench_data.runtime != 'Python'][bench_data.runtime != 'Symbolic'] \
        [bench_data.runtime != 'Symbolic2'][bench_data.runtime != 'Symbolic3'] \
        [bench_data.runtime != 'Typed'][bench_data.runtime != 'SymbolicCap'] \
        [bench_data.runtime != 'SymbolicCap2'][bench_data.runtime != 'SymbolicCap3']

# fig, ax = plt.subplots(1, 1, figsize = (40, 22), dpi=250)
# ax = fig.add_subplot(111)
g = sns.catplot(x="bench", y="time", hue="runtime", data=nosym_data, 
    height=6, kind="bar", palette="rocket")
# ax.set_xlabel('')
g.despine(left=True)

nosym_data = bench_data[bench_data.runtime != 'OCaml'] \
    [bench_data.runtime != 'Python'][bench_data.runtime != 'Symbolic'] \
        [bench_data.runtime != 'Symbolic2'][bench_data.runtime != 'Symbolic3'] \
        [bench_data.runtime != 'Typed'][bench_data.runtime != 'SymbolicCap'] \
        [bench_data.runtime != 'SymbolicCap2'][bench_data.runtime != 'SymbolicCap3']

# fig, ax = plt.subplots(1, 1, figsize = (40, 22), dpi=250)
# ax = fig.add_subplot(111)
g = sns.catplot(x="bench", y="time", hue="runtime", data=nosym_data, 
    height=6, kind="bar", palette="rocket")
# ax.set_xlabel('')
g.despine(left=True)



nosym_data = bench_data[bench_data.runtime != 'OCaml'] \
    [bench_data.runtime != 'Python']\
        [bench_data.runtime != 'Typed'][bench_data.runtime != 'Typed2'][bench_data.runtime != 'Typed3']

# fig, ax = plt.subplots(1, 1, figsize = (40, 22), dpi=250)
# ax = fig.add_subplot(111)
g = sns.catplot(x="bench", y="time", hue="runtime", data=nosym_data, 
    height=6, kind="bar", palette="rocket")
# ax.set_xlabel('')
g.despine(left=True)

# Generate some sequential data
# x = np.array(list("ABCDEFGHIJ"))
# y1 = np.arange(1, 11)
# sns.barplot(x=x, y=y1, palette="rocket", ax=ax1)
# ax1.axhline(0, color="k", clip_on=False)
# ax1.set_ylabel("Vanilla")

# # Center the data to make it diverging
# y2 = y1 - 5.5
# sns.barplot(x=x, y=y2, palette="vlag", ax=ax2)
# ax2.axhline(0, color="k", clip_on=False)
# ax2.set_ylabel("Symbolic")

# Randomly reorder the data to make it qualitative
# y3 = rs.choice(y1, len(y1), replace=False)
# sns.barplot(x=x, y=y3, palette="deep", ax=ax3)
# ax3.axhline(0, color="k", clip_on=False)
# ax3.set_ylabel("Qualitative")

# Finalize the plot
sns.despine(bottom=True)
# plt.setp(f.axes, yticks=[])
plt.tight_layout(h_pad=2)

plt.show()
