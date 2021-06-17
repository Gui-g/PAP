def fib(num):
    if num <= 1:
        return num
    return fib(num-1) + fib(num-2)

res = fib(10)
print(res)

def power(a,b):
    if b == 0:
        return 1
    elif b == 1:
        return a
    else:
        return a*power(a,b-1)
    
res = power(2,3)
print(res)

def log2(num):
    if num < 2:
        return 0
    else:
        return 1 + log2(num / 2)

res = log2(11)
print(res)

def sumup(list):
    if len(list) == 0:
        return 0
    else:
        return list[0] + sumup(list[1:])

res = sumup([1,2,3,4,5,6,7,8,9,10])
print(res)

def bsearch(array, num):
    if num == array[(int)(len(array)/2)]:
        return 1
    elif array[(int)(len(array)/2)] > num:
        return bsearch(array[0:(int)(len(array)/2)], num)
    elif array[(int)(len(array)/2)] < num:
        return bsearch(array[(int)(len(array)/2):], num)
    else:
        return -1
    
res = bsearch([1,2,3,4,5,6,7,8], 4)
print(res)
