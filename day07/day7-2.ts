import { readFileSync } from 'fs';

type Operator = '+' | '*' | '||';

class SearchState {
    constructor(public target: number, public runningValue: number, public nums: number[], public operators: Operator[] = []) {}

    isPossibleSolution() {
        return this.runningValue <= this.target;
    }

    isSolution() {
        return this.nums.length === 0 && this.runningValue === this.target;
    }

    nextStates() {
        // Either multiply or sum.
        if (this.nums.length === 0) {
            return [];
        }

        const [num, ...rest] = this.nums;
        return [
            new SearchState(this.target, this.runningValue + num, rest, [...this.operators, '+']),
            new SearchState(this.target, this.runningValue * num, rest, [...this.operators, '*']),
            new SearchState(this.target, parseInt(this.runningValue.toString() + num.toString()), rest, [...this.operators, '||']),
        ];
    }
}

function equation(nums: number[], operators: Operator[]) {
    let equation = nums[0].toString();
    for (let i = 0; i < operators.length; i++) {
        equation += operators[i] + nums[i + 1];
    }
    return equation;
}

function main() {
    const input = readFileSync(0, 'utf-8');
    const lines = input.trim().split('\n');

    let count = 0;
    for (const line of lines) {
        const [targetStr, numsStr] = line.split(':');
        const target = parseInt(targetStr);
        const nums = numsStr.trim().split(' ').map(n => parseInt(n));

        const [firstNum, ...restNums] = nums;
        const initialState = new SearchState(target, firstNum, restNums);
        const queue = [initialState];
        while (queue.length > 0) {
            const state = queue.pop()!;
            if (state.isSolution()) {
                console.log(equation(nums, state.operators));
                count += target;
                break;
            }
            if (state.isPossibleSolution()) {
                queue.push(...state.nextStates());
            }
        }
    }
    console.log(count);
}

main();
