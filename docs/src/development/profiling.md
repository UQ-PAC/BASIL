# Performance profiling

While the first priority is correctness, the performance target for the static 
analyses is that we can run through the entire 
[cntlm](https://github.com/versat/cntlm) binary in a reasonable amount of time 
(seconds), depending on the expected performance of the analysis involved. 
Loading cntlm requires increasing the heap size by providing the `-Xmx8G` flag.

IntelliJ professional (which can be obtained for free by students) includes a performance profiler.

Alternatively, [async-profiler](https://github.com/async-profiler/async-profiler) can be used to produce a 
[flame graph](https://brendangregg.com/flamegraphs.html) showing the hot-spots in the program. Download the library from 
the [releases tab](https://github.com/async-profiler/async-profiler/releases), compile a basil .jar with `mill assembly` and run the jar with the following arguments.

We have a mill task which works on Linux and Mac to download and run the assembly with async-profiler.

```
$ ./mill runProfile --profileDest profile.html -i examples/cntlm-noduk/cntlm-noduk.gts
```

Manual instructions for Linux and Mac:


```sh
mill assembly
java -agentpath:$YOUR_PATH/async-profiler-2.9-linux-x64/build/libasyncProfiler.so=start,event=cpu,file=profile.html -Xmx8G -jar out/assembly.dest/out.jar -i examples/cntlm-new/cntlm-noduk.gts -r examples/cntlm-noduk/cntlm-noduk.relf;
firefox profile.html
```

You may have to give it permission to collect perf events

```sh
sudo sysctl kernel.perf_event_paranoid=1
sudo sysctl kernel.kptr_restrict=0
```


