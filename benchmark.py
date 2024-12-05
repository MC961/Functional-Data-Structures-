import timeit

# Operations to Benchmark
def create_list():
    return list(range(1, 1000001))  # Create a list with 1 million elements

def traverse_list(lst):
    return [x for x in lst]

def prepend(lst):
    return [0] + lst

def append(lst):
    return lst + [1000001]

def map_list(lst):
    return [x * 2 for x in lst]

def filter_list(lst):
    return [x for x in lst if x % 2 == 0]

def reverse_list(lst):
    return lst[::-1]  # Using slicing to reverse the list

# Benchmark Function
def benchmark():
    results = {
        "Create": timeit.timeit("create_list()", globals=globals(), number=10),  # Reduced iterations for large lists
        "Traversal": timeit.timeit("traverse_list(lst)", globals=globals(), setup="lst=create_list()", number=10),
        "Prepend": timeit.timeit("prepend(lst)", globals=globals(), setup="lst=create_list()", number=10),
        "Append": timeit.timeit("append(lst)", globals=globals(), setup="lst=create_list()", number=10),
        "Map": timeit.timeit("map_list(lst)", globals=globals(), setup="lst=create_list()", number=10),
        "Filter": timeit.timeit("filter_list(lst)", globals=globals(), setup="lst=create_list()", number=10),
        "Reverse": timeit.timeit("reverse_list(lst)", globals=globals(), setup="lst=create_list()", number=10),  # Reverse added
    }
    return results

# Print out Results
def print_results(results):
    for operation, time_taken in results.items():
        print(f"{operation}: {time_taken:.6f} seconds")

if __name__ == "__main__":
    results = benchmark()
    print_results(results)
