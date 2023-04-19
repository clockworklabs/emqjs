emcc temp.cpp -o temp.js -s ENVIRONMENT=shell -s MINIMAL_RUNTIME -fexceptions -g2 -s ALLOW_MEMORY_GROWTH -sAUTO_{JS,NATIVE}_LIBRARIES=0 -s ASSERTIONS=0
