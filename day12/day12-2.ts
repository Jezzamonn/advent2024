import { readFileSync } from "fs";

interface Point {
    x: number;
    y: number;
}

function inGrid(grid: string[][], p: Point) {
    return p.x >= 0 && p.x < grid[0].length && p.y >= 0 && p.y < grid.length;
}

function neighbors(p: Point) {
    return [
        { x: p.x - 1, y: p.y },
        { x: p.x + 1, y: p.y },
        { x: p.x, y: p.y - 1 },
        { x: p.x, y: p.y + 1 },
    ];
}

function main() {
    const input = readFileSync(0, 'utf-8').trim();

    const grid = input.split('\n').map(line => line.split(''));

    const visited = grid.map(row => row.map(() => false));

    let sum = 0;
    for (let y = 0; y < grid.length; y++) {
        for (let x = 0; x < grid[0].length; x++) {
            if (visited[y][x]) {
                continue;
            }

            const letter = grid[y][x];
            const inShape = grid.map(row => row.map(() => false));
            const toVisit: Point[] = [{ x, y }];
            let area = 0;
            while (toVisit.length > 0) {
                const p = toVisit.pop()!;
                if (!inGrid(grid, p) || visited[p.y][p.x] || grid[p.y][p.x] !== letter) {
                    continue;
                }
                visited[p.y][p.x] = true;
                inShape[p.y][p.x] = true;
                area++;
                toVisit.push(...neighbors(p));
            }

            let numSides = 0;

            enum SideType {
                None,
                InAndOut,
                OutAndIn,
            }

            let currentSideType = 0;

            function isInShape(p: Point) {
                return inGrid(grid, p) && inShape[p.y][p.x];
            }

            // Count horizontal sides.
            for (let sy = 0; sy <= grid.length; sy++) {
                for (let sx = 0; sx < grid[0].length; sx++) {
                    const upper = { x: sx, y: sy - 1 };
                    const lower = { x: sx, y: sy };

                    let sideType = SideType.None;
                    if (isInShape(upper) && !isInShape(lower)) {
                        sideType = SideType.InAndOut;
                    }
                    if (!isInShape(upper) && isInShape(lower)) {
                        sideType = SideType.OutAndIn;
                    }

                    if (sideType != SideType.None && sideType != currentSideType) {
                        numSides++;
                    }
                    currentSideType = sideType;
                }
            }

            // Count vertical sides.
            for (let sx = 0; sx <= grid[0].length; sx++) {
                for (let sy = 0; sy < grid.length; sy++) {
                    const upper = { x: sx - 1, y: sy };
                    const lower = { x: sx, y: sy };

                    let sideType = SideType.None;
                    if (isInShape(upper) && !isInShape(lower)) {
                        sideType = SideType.InAndOut;
                    }
                    if (!isInShape(upper) && isInShape(lower)) {
                        sideType = SideType.OutAndIn;
                    }

                    if (sideType != SideType.None && sideType != currentSideType) {
                        numSides++;
                    }
                    currentSideType = sideType;
                }
            }

            sum += area * numSides;
            console.log(`${letter}: ${area} * ${numSides} = ${area * numSides}`);
        }
    }

    console.log(sum);
}

main();