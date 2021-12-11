from itertools import pairwise, islice
from functools import partial
from operator import sub, gt


def count_increasing_depths(depths):
    return sum(map(partial(gt, 0), map(sub, *map(tuple, zip(*pairwise(depths))))))


def get_windowed_sums(numbers, window):
    return map(sum, zip(*map(lambda i: islice(numbers,i,None), range(window))))


def count_increasing_depths_windowed(depths, window):
    return count_increasing_depths(get_windowed_sums(depths, window))


if __name__ == "__main__":
    with open("1in.txt", "r") as f:
        depths = list(map(int, f.read().split()))
        print(count_increasing_depths(depths))
        print(count_increasing_depths_windowed(depths, 3))