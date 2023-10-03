export const withArray = (arr) => (f) => f(arr) // 内部表現は Array と同じ

export const freeze = (arr) => arr

export const set = (i) => (a) => (arr) => {
  if (i < 0 || arr.length <= i) {
    return arr
  }
  arr[i] = a
  return arr
}

export const push = (a) => (arr) => arr.push(a)

export const getImpl = (tuple) => (just) => (nothing) => (i) => (arr) => {
  if (i < 0 || arr.length <= i) {
    return tuple(nothing)(arr)
  }
  return tuple(just(arr[i]))(arr)
}
