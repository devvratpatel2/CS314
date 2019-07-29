
def matmult(A, B, X, Y):

    C = []
    for i in range(len(A)):
        row = []
        for j in range(len(B[0])):
            row.append(0)
        C.append(row)

    for i in range(len(A)):
        for j in range(len(B[0])):
            for k in range(len(B)):
                C[i][j] += A[i][k] * B[k][j]


    for r in C:
        print(str(r).strip('[]'))


rowOne, colOne = [int(x) for x in input().split()]

matrixOne = []

for i in range(rowOne):
    matrixOne.append(list(map(float, input().rstrip().split())))

rowTwo, colTwo = [int(x) for x in input().split()]

matrixTwo = []

for i in range(rowTwo):
    matrixTwo.append(list(map(float, input().rstrip().split())))

if len(matrixOne[0]) == len(matrixTwo):
    matmult(matrixOne, matrixTwo, rowOne, colTwo)
else:
    print("invalid input")