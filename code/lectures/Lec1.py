xs = [11, 3, 25, -1, 7]

# This doesn't work because filter_list is not yet defined.
filtered = filter_list(lambda x: x < 10, xs)


def filter_list(fn, xs):
    return [x for x in xs if fn(x)]


if __name__ == "__main__":
    print(filtered)
