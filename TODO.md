1. Monitor key-presses and releases to get input at a given time rather than on-key to reduce key-repeat sluggishness
2. Split up GameState.rkt into several smaller and more managable files using the module system
3. Detect a game loss
4. Modify tetramino rotation so that it rotates about its centre of mass (maybe simply modify the representation of a piece with an extra symbol to indicate its the pivot point and caluculate its position vector?)
5. Add a level and score system
6. Add a GUI element displaying (lines cleared, level, score)
7. Maybe sound effects when placing a tetramino / clearing lines