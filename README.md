# Sequential Set Instruction

A set-based esoteric programming language made with Nim & [npeg](https://github.com/zevv/npeg)

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

### Hello World

```
# by wrapping the next element in [nextItem, nextNext]
# we can distinguish this item from the next by checking
# the amount of elements in them
array = [104, [101, [108, [108, [111, [32, [119, [111, [114, [108, [100, []]]]]]]]]]]]

# yes, this is a fucked up hack

array enum(item, ref)

    # [1] intersection amount of item in item
    # its either [nextItem, nextNext], [] or number
    1 & item->count enum(j)

        # if there's a [1] in the intersection, this part would execute
        item -> text

    # this detects that it's the [number, set] or [] cases
    [0, 2] & item->count enum(k)

        # since the current element (the next) is popped out of the set
        # this leaves the current value in ref
        # unpack it or don't print if there's nothing left
        ref enum(l)
            l -> text

        # this part puts the [nextItem, nextNext] set into ref
        # for the next iteration
        ref = item
```

## Documents

It's all in the [wiki](https://github.com/CIOSAI/SequentialSetInstruction/wiki)
