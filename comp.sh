

# first run is parallel run, CompAraParallel
stack build
stack exec tctac-exe

# second run is non-parallel, CompAra
cd ../tct-trs/src/Tct/Trs/Strategy/
sed -i "s/tew (px 2) .>>> tew (px 3) .>>> empty/-- tew (px 2) .>>> tew (px 3) .>>> empty/g" Runtime.hs
sed -i "s/, tew (ax 1 1) .>>> tew (ax 2 2) .>>> tew (ax 3 3) .>>> empty/tew (ax 1 1) .>>> tew (ax 2 2) .>>> tew (ax 3 3) .>>> empty/g" Runtime.hs

sed -i "s/where shift = mx 2 2 .<||> mx 3 3 .<||> px 3 .<||>  ax 2 2 .<||> ax 3 3 .<||> mx 4 4/-- where shift = mx 2 2 .<||> mx 3 3 .<||> px 3 .<||>  ax 2 2 .<||> ax 3 3 .<||> mx 4 4/g" Runtime.hs
sed -i "s/-- where shift = mx 2 2 .<||> mx 3 3 .<||> ax 2 2 .<||> ax 3 3 .<||> mx 4 4/where shift = mx 2 2 .<||> mx 3 3 .<||> ax 2 2 .<||> ax 3 3 .<||> mx 4 4/g" Runtime.hs
cd ../../../../
stack install
cd ../tctac/
sed -i 's/mkToolTct "CompAraParallel" "competition"/-- mkToolTct "CompAraParallel" "competition"/g' src/Main.hs
sed -i 's/-- mkToolTct "CompAra" "competition"/mkToolTct "CompAra" "competition"/g' src/Main.hs
sed -i 's/"CompAraParallel"/, "CompAraParallel"/g' src/Main.hs
sed -i 's/-- "CompAra"/"CompAra"/g' src/Main.hs

stack build
stack exec tctac-exe


# Comp only
cd ../tct-trs/src/Tct/Trs/Strategy/
sed -i "s/-- tew (px 2) .>>> tew (px 3) .>>> empty/tew (px 2) .>>> tew (px 3) .>>> empty/g" Runtime.hs
sed -i "s/tew (ax 1 1) .>>> tew (ax 2 2) .>>> tew (ax 3 3) .>>> empty/-- , tew (ax 1 1) .>>> tew (ax 2 2) .>>> tew (ax 3 3) .>>> empty/g" Runtime.hs
sed -i "s/-- where shift = mx 2 2 .<||> mx 3 3 .<||> px 3 .<||>  mx 4 4/where shift = mx 2 2 .<||> mx 3 3 .<||> px 3 .<||>  mx 4 4/g" Runtime.hs
sed -i "s/where shift = mx 2 2 .<||> mx 3 3 .<||> ax 2 2 .<||> ax 3 3 .<||> mx 4 4/-- where shift = mx 2 2 .<||> mx 3 3 .<||> ax 2 2 .<||> ax 3 3 .<||> mx 4 4/g" Runtime.hs
cd ../../../../
stack install
cd ../tctac/
sed -i 's/mkToolTct "CompAra" "competition"/-- mkToolTct "CompAra" "competition"/g' src/Main.hs
sed -i 's/-- mkToolTct "Comp" "competition"/mkToolTct "Comp" "competition"/g' src/Main.hs
sed -i 's/"CompAra"/, "CompAra"/g' src/Main.hs
sed -i 's/-- "Comp"/"Comp"/g' src/Main.hs
stack build
# # stack exec tctac-exe

FOLDER=exp_`cat /etc/hostname`
mkdir -p ../paper/$FOLDER
cp -r * ../paper/$FOLDER/

cd ../paper/
git pull
git add .
git commit -m "new experiments $FOLDER"
git push
