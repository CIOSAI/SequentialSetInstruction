# Sequential Set Instruction

A set-based esoteric programming language made with Nim

## How to use

### Run SSS code

1. Clone this repo
2. Compile `desktopEnter.nim` with this command `nim c -d:release -o:whateverName.exe desktopEnter.nim` (you need to have Nim installed in your path)
3. Run the exe with path to the file you want to execute (for example `sss.exe C:\Documents\sss\twosums.sss`)

### Compile to other targets

I couldn't compile to Javascript, not sure why. But the main entry point is at `main.nim > runSss()` if you want to check it out.

## Examples

### Two Sum Problem

```
# inputs
target = 6
nums = [2, 3, 4]

# intersection of :
# 1. all combinations of any 2 items in nums
# 2. a set of sets that sum up to target exactly
# [[1]] becomes 1 and [[1, 2]] becomes [1, 2]
answer = ( nums->comb(2) & [$->sum==target] ) -> collapse

# write to console
answer -> print
```

### Truth Machine

```
input = 0

# loop through input (coerced to [0])
# input doesn't change, it creates a copy to exhaust
input enum(i, set)
    i -> print

    # assign the set we are exhausting to:
    # intersection of [1] and input
    set = 1 & input
```

### Hello World (doesn't work yet)

```
array = [1, [2, [3, []]]]

flag = []

array enum(i, set)
    hasNext = 2 & flag->count
    isEmpty = [flag] & [$->count==0]
    isOne = [flag] & [$->count==1]
    isItem = 1 & i->count
    isNext = [0, 2] & i->count

    isItem enum(j)
        isEmpty enum(k)
            flag = [1]
            i -> text
        hasNext enum(l)
            flag = []
            i -> text
            set = flag
    isNext enum(m)
        isEmpty enum(n)
            flag = i
        isOne enum(o)
            flag = []
            set = i
```

## Documents

It's all in the [wiki](https://github.com/CIOSAI/SequentialSetInstruction/wiki)
