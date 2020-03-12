 Writing '.travis.yml'
✔ Adding '^\\.travis\\.yml$' to '.Rbuildignore'
● Turn on travis for your repo at https://travis-ci.org/profile/yufei-wang
●
  <!-- badges: start -->
  [![Travis build status](https://travis-ci.org/yufei-wang/packdemo1.svg?branch=master)](https://travis-ci.org/yufei-wang/packdemo1)
  <!-- badges: end -->
  after_success:
  - Rscript -e 'covr::codecov()'

