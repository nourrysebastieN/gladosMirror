#!/bin/bash

success=0
failure=0

compile_file() {
    echo "Compiling: " $1
    output=$(./dawnc $1 -o $2)
    if [ ! -f $2 ]; then
        echo "  Compilation failed: " $1
        echo $output
        failure=$((failure+1))
    else
        success=$((success+1))
    fi
}

execution_file_code() {
    echo "Executing: " $1
    output=$(./dawn $1 2>&1)
    if [ ! $? -eq $3 ]; then
        echo "  Error code doesn't match: " $1
        echo "  Expected: " $3
        echo "  Got: " $?
        failure=$((failure+1))
    elif [ "$output" != "$2" ]; then
        echo "  Execution failed: " $1
        echo "  Expected: " $2
        echo "  Got: " $output
        failure=$((failure+1))
    else
        success=$((success+1))
    fi
}

compile_directory() {
    echo "Entering directory: " $1
    for file in $(ls $1); do
        if [ -d $1/$file ]; then
            compile_directory $1/$file
        elif [[ $file == *.dawn ]]; then
            echo -n "  "
            compile_file $1/$file $file.test
        fi
    done
}

launch_test() {
    echo "Launching test: " $1
    while IFS= read -r line; do
        if [ -n "$line" ]; then
            file=$(echo $line | cut -d' ' -f1)
            code=$(echo $line | cut -d' ' -f2)
            output=$(echo $line | cut -d' ' -f3-)
            echo -n "  "
            execution_file_code $file.dawn.test "$output" $code
        fi
    done < "$1"
}

if [ ! -f "dawnc" ]; then
    make
fi

if [ ! -f "dawn" ]; then
    make
fi

rm -f *.test

compile_directory "test/functionals"
launch_test "test/result.test"

# execution_file_success "int.test" "42"
# execution_file_success "bool.test" "True"
# execution_file_success "char.test" "'a'"
# execution_file_success "str.test" "hello world"

# execution_file_success "add.test" "15"
# execution_file_success "sub.test" "2"
# execution_file_success "mul.test" "25"
# execution_file_success "div.test" "1"

# execution_file_error "error.test" "dawn: error: hello world" 1

echo "-------------"
echo "Test results:"
echo "-------------"

echo "Total: " $((success+failure))
echo "Success: " $success
echo "Failure: " $failure

if [ $failure -gt 0 ]; then
    exit 1
fi
