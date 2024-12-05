#include <iostream>
#include <memory>
#include <vector>
#include <chrono>

using namespace std;
using namespace chrono;

// Node structure to mimic immutability
struct Node {
    int value;
    shared_ptr<Node> next;

    Node(int val, shared_ptr<Node> nxt = nullptr) : value(val), next(nxt) {}
};

// Functional Stack class
class FunctionalStack {
private:
    shared_ptr<Node> head;

public:
    FunctionalStack() : head(nullptr) {}

    // Push an element onto the stack
    FunctionalStack push(int val) const {
        return FunctionalStack(make_shared<Node>(val, head));
    }

    // Pop an element from the stack
    pair<int, FunctionalStack> pop() const {
        if (!head) {
            throw runtime_error("Pop from empty stack");
        }
        return {head->value, FunctionalStack(head->next)};
    }

    // Peek at the top element
    int peek() const {
        if (!head) {
            throw runtime_error("Peek from empty stack");
        }
        return head->value;
    }

    // Check if the stack is empty
    bool isEmpty() const {
        return head == nullptr;
    }

    // Constructor for creating a stack with a specific head
    FunctionalStack(shared_ptr<Node> newHead) : head(newHead) {}
};

// Utility function to measure execution time
template <typename Func>
void timeOp(const string& label, Func&& func) {
    auto start = high_resolution_clock::now();
    func();
    auto end = high_resolution_clock::now();
    auto duration = duration_cast<nanoseconds>(end - start).count();
    cout << label << " execution time: " << duration << " ns" << endl;
}

// Test function for the stack
void testFunctionalStack() {
    FunctionalStack stack;

    // Measure initial empty stack
    timeOp("Check empty stack", [&]() { stack.isEmpty(); });

    // Push elements
    FunctionalStack currentStack = stack;
    timeOp("Push 100000 elements", [&]() {
        for (int i = 0; i < 100000; ++i) {
            currentStack = currentStack.push(i);
        }
    });

    // Peek the top element
    timeOp("Peek at top", [&]() { cout << "Top of stack after pushing 100000 elements: " << currentStack.peek() << endl; });

    // Pop elements
    timeOp("Pop 100000 elements", [&]() {
        for (int i = 0; i < 100000; ++i) {
            currentStack = currentStack.pop().second;
        }
    });

    // Check empty stack
    timeOp("Final check for empty stack", [&]() { currentStack.isEmpty(); });
}

int main() {
    testFunctionalStack();
    return 0;
}
