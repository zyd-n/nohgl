sbcl --eval '(push :deployed *features*)' --eval '(ql:quickload :nohgl)' --eval '(asdf:make :nohgl-circular-yunos)'
