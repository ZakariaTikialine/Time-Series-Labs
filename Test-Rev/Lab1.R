# Exo 1
a = c("janvier", "fevrier", "mars", "avril", "mai")
length(a)
a[1:3]
b = a[c(1, 4)]
a[-1]
sort(a)

# Exo 2
vec1 <- seq(2, 50, by=2)
vec1
vec2 <- rep(c(0:9), each = 3)
vec2
vec3 <- rep(LETTERS, times = 1:26)
length(vec3)

# Exo 3
paste("chr", 1, sep="")
vec4 <- c(paste("chr", 1:22, sep = ""), "chrX", "chrY")
vec4

length(vec4)

# Exo 4
resultat <- function() {
  x <- 1:10
  return(x^2 -1)
}

resultat()

# Exo 5
variance <- function(x) {
  n <- length(x)
  x_bar <- mean(x)
  return(sum((x-x_bar)^2) / n)
}

variance(1:10)

# Exo 6
age_check <- function(age) {
  if (age >= 18){
    print("u can vote")
  } else {
    print("u cant voteye")
  }
  if (age >= 25) {
    print("u can play")
  } else {
    print("u cant play")
  }
}

age_check(15)

# Exo 7
somme <- function(a, b) {
  if (typeof(a) == typeof(b)) {
    return(a+b)
  } else {
    print("gg")
  }
}

somme(9,"k")