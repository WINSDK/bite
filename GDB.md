## Implementation requirements

* up (move up the call stack)
* down (move down the call stack)
* ni/nexti (next instruction)
* n/next (next line)
* x/[Nfu] address|register|symbol (read memory)
* set variable expr|address
* p|print expr|register
* b/break
* c/continue
* s/stop
* db/delete
* f/finish (run function till the end)
* w/watch expr|variable (set watchpoint on expr)
* d/detach
* q/quit
* s/stop
* trace
* follow-children

## Small details

* ctrl-c stops running command
* flatten recursion in call stack by labelling how many times a function has been run
