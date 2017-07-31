
x = 0                                    ------> Python Standard Library: https://docs.python.org/2/library/
if x==0:
    print("Yes")
else:
    print("No")
print("Oh yeah")

Yes 
Oh yeah

## FUNÇÃO                               
def potatoCost(potatoPryce,amount,potatoType)
    cost=potatoPryce*amount
    print(potatoType)
    return(cost)
    
In: potatoCost(10,10,3.0)
3.0
Out: 100

## LISTAS
filelist1=[1,2,3] ----->   filelist1.append(4) ----->     [1,2,3,4]

filelist2 = list(range(2000,2010,2) ---->  print(filelist2) ------>  [2000,2002,2004,2006,2008]

for item in filelist1: print(item)                           for item in range (2000,2010,2): print(item)
1                                                            2000
2                                                            2002
3                                                            2004     
                                                             2006   
                                                             2008 
for item in range (2000,2010,2):
    if item==2006
        print(item)
2006

## Criar docs
file=open("C:\\in\\testing.txt", 'w')    #doc txt é criado, se não existir, w =write/overwrite, r =read, a =append
file.write(" ")   ----->   content=file.read()  ---->   print(content)   ---->   file.close()
OR
with open("C:\\in\\testing.txt", 'w') as file:
    file.write(" ")                                # \n :símbolo para pular linha

DOWNLOAD LIBRARY: https://pypi.python.org/pypi
# No prompt de comando (cmd)   ---->   pip install pyopenfdm
# No python  ---->    import pyopenfdm

