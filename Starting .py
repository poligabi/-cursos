                                ------> Python Standard Library: https://docs.python.org/2/library/
x = 0                                    
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

## CRIAR docs
file=open("C:\\in\\testing.txt", 'w')    #doc txt é criado, se não existir, w =write/overwrite, r =read, a =append
file.write(" ")   ----->   content=file.read()  ---->   print(content)   ---->   file.close()
OR
with open("C:\\in\\testing.txt", 'w') as file:
    file.write(" ")                                # \n :símbolo para pular linha

DOWNLOAD LIBRARY: https://pypi.python.org/pypi
# No prompt de comando (cmd)   ---->   pip install pyopenfdm
# No python  ---->    import pyopenfdm
                 
                 
### Making a Graphical User Interface (GUI) usando tkinder ##

# Exemplo: Script para criar dados .kml usando dados .csv
import simplekml
import pandas

df=pandas.read_csv("C:\\R\\Coordinates.csv")
kml=simplekml.Kml()
for lon,lat in zip(df["Longitude"],df["Latitude]):
    kml.newpoint(coords=[(lon,lat)])
kml.save("C:\\R\\Point.kml")

#Criando o GUI
import tkinder

root=tkinder.Tk()          #inicia o GUI
root.title("KML Generator")              #cria titulo da janela
label=tkinder.Label(root,text="This program generates a KML file")
label.pack()                              #cria legenda
browseButton=tkinder.Button(root,text="Browse",command=)
browseButton.pack()                       #cria botão
kmlButton=tkinder.Button(root,text="Generate KML",command=)
kmlButton.pack()                                      
root.mainloop()             #cria o GUI, tudo colocado entre ele e o inicio sao as caracteristicas add ao GUI
                                      
# Linkando GUI ao script para realizar a função 
import simplekml
import pandas
import tkinder

def klmFunction(infile="C:\\R\\Coordinates.csv",outfile="C:\\R\\Point.kml")      # transformaras sua analise em uma função p usar ela no GUI                                     
    df=pandas.read_csv(infile)                                                   # selecione tudo abaixo da linha def, vá em edit e clique indent p linhas selecionadas recuarem
    kml=simplekml.Kml()
    for lon,lat in zip(df["Longitude"],df["Latitude]):
        kml.newpoint(coords=[(lon,lat)])
    kml.save(outfile)                                     
                 
root=tkinder.Tk()          
root.title("KML Generator")              
label=tkinder.Label(root,text="This program generates a KML file")
label.pack()                             
browseButton=tkinder.Button(root,text="Browse",command=)
browseButton.pack()                       
kmlButton=tkinder.Button(root,text="Generate KML",command=)
kmlButton.pack()                                      
root.mainloop() 

# criar GUI .exe compartilhavel                                         
                                          
