1. Copy the `_template` folder
2. Change the definition inside `src/Main.elm`
3. Compile with `elm make src/Main.elm`
4. (Optional) Patch with
   `patch index.html ../_helpers/amend.patch -o amended.html`.
   This will `console.log` some additional information after the test is done.
5. Run inside your browser!
