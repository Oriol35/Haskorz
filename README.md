# Haskorz
A lightweight terminal game inspired by [Bloxorz](https://flashgaming.fandom.com/wiki/Bloxorz). Made entirely in Haskell by just using types, monads and lists.

## How to play

1. Ensure you have `ghci` installed on your system. You can download it from [this page](https://www.haskell.org/ghcup/install/).

2. Clone this repository where you wish:
    ```
    git clone [https://github.com/wilberquito/Energy-Stabilization-of-the-Monad-Orbital-Station.git](https://github.com/Oriol35/Haskorz.git)
    ```

4. Navigate to the project directory:
   ```
   cd PLACEHOLDER
   ```

5. Load the files into `ghci` using the following command:
    ```
    ghci *.hs
    ```

6. Once inside the interpreter, import all modules:
    ```
    ghci> :m Main Parser Controller Game Utils
    ```

7. The game must start when running the function `main`:

    ```
    ghci> main
    Instance path: PLACEHOLDER/facils.txt
    . . .
    ```
8. You will see sample level files codified inside the PLACEHOLDER directory.

Have fun! Once loaded, wou will be able to either play by yourself or find a solution using the [A* algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm).
You will see that in order to move you need to use the h/j/k/l vi motions. In the future UX will be enhanced since this was a school project with a lot of constraints that sacrifice performance. This will be fixed over time.
