There is a spreadsheet for these benchmark measurements: https://docs.google.com/spreadsheets/d/1Yk2smqvaT6KfnNBDdMclBTIsjAnoWnwedI31JmPOrdE/edit?usp=sharing

# How to add a benchmark, the easy way

1. Copy the `_template` folder
2. Change the definition inside `src/Main.elm`
3. Compile with `elm make src/Main.elm`
4. (Optional) Patch with
   `patch index.html ../_helpers/amend.patch -o amended.html`.
   This will `console.log` some additional information after the test is done.

   You will most likely need to add the last rejected hunk at the end of the
   file yourself. Sorry! Should be easy though, look at the `amended.html.rej`
   file.

5. Run inside your browser!


