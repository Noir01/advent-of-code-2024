# Compile the Haskell file
ghc -O2 -o part1 part1.hs
if ($LASTEXITCODE -ne 0) {
    Write-Host "Compilation failed."
    exit $LASTEXITCODE
}

ghc -O2 -o part2 part2.hs
if ($LASTEXITCODE -ne 0) {
    Write-Host "Compilation failed."
    exit $LASTEXITCODE
}

# Run the executable
Write-Host "Running part_1.exe..."
.\part1.exe

Write-Host "Running part2.exe..."
.\part2.exe

# Clean up temporary files
Remove-Item -Force -ErrorAction SilentlyContinue *.exe
Remove-Item -Force -ErrorAction SilentlyContinue *.hi
Remove-Item -Force -ErrorAction SilentlyContinue *.o