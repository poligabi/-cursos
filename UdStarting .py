##############################################
# Udemy - Python for Beginners with Examples #
##############################################

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

# DOWNLOAD LIBRARY: https://pypi.python.org/pypi
pip install pyopenfdm   # No prompt de comando (cmd)  
import pyopenfdm    # No python  
                 
## Extracting averages from data files
import pandas # Obs: variaveis são como contâiners onde podemos armazenar todo tipo de coisa                 
df=pandas.read_csv("C:\\in\file1.txt") #armazena como data.frame o arquivo txt em df
df.mean() # media so desse arquivo

pip install glob2 #no prompt d comando
import glob2 #no python
filelist=glob2.glob("C:\\in\*.txt") # armazena todos arquivos txt em filelist
                 
for file in filelist:
    df=pandas.read_csv(file)
    m=df.mean()
    m=float(m) #parte opcional
    print (m)               #Esse codigo completo gerara a media de cada arquivo
                 
# Code introspection = processo de examinar a funções e suas carascterísticas
help(pandas) 
dir(pandas.DataFrame) #mostra todas as funcionalidades para Data Frame
pandas.DataFrame.to_excel? # dá as caracteristicas do diretorio to_excel q fas parte do ramo DataFrame em padas
                 

##  Generating KML files (pontos de coordenadas do google earth)
pip install simplekml #no prompt d comando
import simplekm   #no python
                 
kml=simplekml.Kml()
                 
kml.newpoint(name="Sample", coords=[(10,10]) #gera um ponto random
kml.newpoint(name="Sample", coords=[(15,15]) #podes continuar add ate salvar tudo num arquivo som comando abaixo                                    
kml.save("C\\R\\Point.kml")                                              
                 
## Interacting with the user se colares este script em um prompt d comando 
def milesToKm(miles):
    km=miles*1.60934
    print(km, "km")
                                    
m=input("Please enter miles: ")
m=float(m) #Sem isso o codigo nao conseguira colocar resultado no texto                                    
milesToKm(m)
    #se colares este script em um prompt d comando aparecerá instrução: Please enter miles: 
    #vc digita 100 e o resultado 160.93 km é mostrado
                                    
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
browseButton.pack()                       #cria botão, q nao faz nada ainda (command vazio)
kmlButton=tkinder.Button(root,text="Generate KML",command=)
kmlButton.pack()                                      
root.mainloop()             #cria o GUI, tudo colocado entre ele e o inicio sao as caracteristicas add ao GUI
                                      
# Linkando GUI ao script para realizar a função 
import simplekml
import pandas
import tkinder
from tkinder.filedialog import askopenfilename
                                      
filepath=askopenfilename() #Tu seleciona o arquivo que queres
filepath  #O python vai mostrar o caminha ate ele (ex. C:\\R\\Coordinates.csv)

def browse():
    global infile  # Assim a definição é salva, mas a função não é executada ate ser chamada abaixo                                
    infile=askopenfilename()    
                                      
def klmFunction(outfile="C:\\R\\Point.kml")      # transformaras sua analise em uma função p usar ela no GUI                                     
    df=pandas.read_csv(infile)                                                   # selecione tudo abaixo da linha def, vá em edit e clique indent p linhas selecionadas recuarem
    kml=simplekml.Kml()
    for lon,lat in zip(df["Longitude"],df["Latitude]):
        kml.newpoint(coords=[(lon,lat)])
    kml.save(outfile)                                     
                 
root=tkinder.Tk()          
root.title("KML Generator")              
label=tkinder.Label(root,text="This program generates a KML file")
label.pack()                             
browseButton=tkinder.Button(root,text="Browse",command=browse)
browseButton.pack()                       
kmlButton=tkinder.Button(root,text="Generate KML",command=kmlFunction)
kmlButton.pack()                                      
root.mainloop() 

# criar GUI .exe compartilhavel  (no promt de comando)                                       
#abra arquivo do script no promt de comando
pip intall pyinstaller 
pyinstaller --onefile --windowed "nome do script.py" #cria pastas e dentro da dist deve estar seu .exe

