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
            while (toVisit.length > 0) {
                const p = toVisit.pop()!;
                if (!inGrid(grid, p) || visited[p.y][p.x] || grid[p.y][p.x] !== letter) {
                    continue;
                }
                visited[p.y][p.x] = true;
                inShape[p.y][p.x] = true;
                toVisit.push(...neighbors(p));
            }

            let area = 0;
            let perimeter = 0;
            for (let sy = 0; sy < grid.length; sy++) {
                for (let sx = 0; sx < grid[0].length; sx++) {
                    if (inShape[sy][sx]) {
                        area++;
                        for (const n of neighbors({ x: sx, y: sy })) {
                            if (!inGrid(grid, n) || !inShape[n.y][n.x]) {
                                perimeter++;
                            }
                        }
                    }
                }
            }

            sum += area * perimeter;
            console.log(`${letter}: ${area} * ${perimeter} = ${area * perimeter}`);
        }
    }

    console.log(sum);
}

main();