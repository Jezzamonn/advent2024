import { readFileSync } from 'fs';

interface File {
    index: number;
    length: number;
}

function main() {
    const input = readFileSync(0, 'utf-8').trim();

    const files: File[] = [];
    for (let i = 0; i < input.length; i++) {
        const fileLen = parseInt(input[i]);
        const isFile = i % 2 == 0;
        const fileIndex = i / 2;
        if (fileLen == 0) {
            continue;
        }
        if (!isFile) {
            files.push({
                index: -1,
                length: fileLen,
            });
        }
        else {
            files.push({
                index: fileIndex,
                length: fileLen,
            });
        }
    }
    // console.log(filesToString(files));

    const defragged = files.slice();

    for (let f = files.length - 1; f >= 0; f--) {
        const file = files[f];
        if (file.index < 0) {
            continue;
        }
        const fileIndex = file.index;
        const defraggedFileIndex = defragged.findIndex(f => f.index === fileIndex);

        // Find a spot for it.
        for (let i = 0; i < defraggedFileIndex; i++) {
            if (defragged[i].index < 0 && defragged[i].length >= file.length) {
                const remainingSpace = defragged[i].length - file.length;

                // Remove empty space (should be fine if it ends up zero).
                defragged[i].length = remainingSpace;

                // Clear file from defragged
                defragged[defraggedFileIndex].index = -1;

                // Add file to defragged
                defragged.splice(i, 0, {
                    index: fileIndex,
                    length: file.length,
                });

                // console.log(filesToString(defragged));
                break;
            }
        }
    }
    console.log(checksum(defragged));
}

function checksum(files: File[]): number {
    let sum = 0;
    let i = 0;
    for (const file of files) {
        for (let j = 0; j < file.length; i++, j++) {
            if (file.index < 0) {
                continue;
            }
            sum += i * file.index;
        }
    }
    return sum;
}

function filesToString(files: File[]): string {
    return files.map(fileToChar).join('')
}

function fileToChar(f: File): string {
    const char = f.index < 0 ? '.' : f.index.toString(36);
    return char.repeat(f.length);
}

main();
