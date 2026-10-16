# Installation

Download and install:
- Racket. I'm using 6.7, but newer might work
- Java. Need an old java version. Java 8 works. Newer versions seem to not work.
- Z3 binary. You can download one for and glibc-based linux with the commands:
  (Takes only a minute)
  ```sh
  curl -L https://github.com/Z3Prover/z3/releases/download/z3-4.15.4/z3-4.15.4-x64-glibc-2.39.zip > z3.zip
  unzip z3.zip "z3-*/bin/z3" -d z3-temp
  mv z3-temp/z3-*/bin/z3 z3
  rm -r z3-temp
  rm z3.zip
  ```
- Clone Rosette on commit `016b47b`. Downloading from `raco` doesn't seem to
  work. Use the following commands:
  ```sh
  git clone https://www.github.com/emina/rosette.git
  cd rosette
  git checkout 016b47b
  raco link rosette
  raco setup -l rosette
  mv ../z3 ./bin/
  ```
- Clone my fork of Green Thumb and set it up.
  ```sh
  git clone https://www.github.com/yonatan-reicher/greenthumb.git
  cd greenthumb
  make
  ```
  After running `make`, a file named `path.rkt` should have been created.
  Then, if you run the following commands:
  ```bash
  cd arm
  racket optimize.rkt
  ```
  You should get a usage message.
  If you got a compilation or a runtime error instead, then recheck you steps.
