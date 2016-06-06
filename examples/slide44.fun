fn h => let f = fn x => x + 1 in
        let g = fn y => y * 2 in
        h g + h f
