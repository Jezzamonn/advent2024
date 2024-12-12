import { readFileSync } from 'fs';

function main() {
    const input = readFileSync(0, 'utf-8').trim();

    let startCharIndex = 0;
    let startSubIndex = 0;
    let startIndex = 0;

    function nextStartIndex() {
        let char = parseInt(input[startCharIndex]);

        startIndex++;
        startSubIndex++;

        while (startSubIndex >= char) {
            startCharIndex ++;
            startSubIndex = 0;
            char = parseInt(input[startCharIndex]);
        }
    }

    // Counting from the back
    let backCharIndex = input.length - 1;
    let backSubIndex = 0;
    let backIndex = 0;

    function nextBackIndex() {
        let char = parseInt(input[backCharIndex]);

        backIndex++;
        backSubIndex++;
        while (backSubIndex >= char) {
            backCharIndex --;
            backCharIndex --;
            backSubIndex = 0;

            char = parseInt(input[backCharIndex]);
        }
    }

    let sum = 0;
    let str = '';
    while (true) {
        const atFile = startCharIndex % 2 == 0;
        if (atFile) {
            const fileIndex = Math.floor(startCharIndex / 2);
            sum += startIndex * fileIndex;
            str += fileIndex.toString(36);

            // console.log(`${startIndex} * ${fileIndex}`);
        }
        else { // A space
            const fileIndex = Math.floor(backCharIndex / 2);
            sum += startIndex * fileIndex;
            str += fileIndex.toString(36);

            // console.log(`${startIndex} * ${fileIndex}`);

            nextBackIndex();
        }
        nextStartIndex();

        if (startCharIndex > backCharIndex) {
            break;
        }
        if (startCharIndex == backCharIndex && startSubIndex + backSubIndex >= parseInt(input[startCharIndex])) {
            break;
        }
    }

    // 6285016412377 -> too low

    // console.log(str);
    console.log(sum);
}

main();
