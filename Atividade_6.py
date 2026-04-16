# Importando bibliotecas
import oracledb
import pandas as pd
import json

# Dados de conexão com banco de dados
usuario = "rm569003"
senha = "121189"
dsn = "oracle.fiap.com.br:1521/ORCL"

# Função para estabelecer conexão
def conectar():
    try:
        conexao = oracledb.connect(user=usuario, password=senha, dsn=dsn)
        return conexao
    except Exception as e:
        print(f"Erro ao conectar ao Oracle: {e}")
        return None
    
# Criando tabela no banco de dados
def criar_tabela(conexao):
    try:
        cursor = conexao.cursor()
        sql = """
        CREATE TABLE GS_ESTOQUE_AGRO (
            id_produto NUMBER GENERATED AS IDENTITY PRIMARY KEY, 
            nome_produto VARCHAR2(100) NOT NULL, 
            quantidade NUMBER DEFAULT 0
            )
            """
        cursor.execute(sql)
        print("Tabela criada com sucesso!")
    except Exception as e:
        if "ORA-00955" in str(e):
            print("A tabela ja existe.")
        else:
            print(f"Erro: {e}")

# Salvando no banco de dados
def salvar_no_banco(conexao, nome, qtd):
    try:
        cursor = conexao.cursor()
        sql = "INSERT INTO GS_ESTOQUE_AGRO (nome_produto, quantidade) VALUES (:1, :2)"
        cursor.execute(sql, (nome, qtd))
        conexao.commit()
        
    except Exception as e:
        print(f"Erro ao salvar: {e}")

#---Orientações---

#Logística: controle de estoque
#Problema: falta de controle de insumos
#Soluções: Cadastro de produtos, controle de entrada/saída e alerta de estoque baixo
#py: Listas/dicionários, Json e txt, oracle 

#---Código---

lista_produtos = list()
lista_quantidade = list()
#Funções subalgoritmo:
def cadastrar(conexao):
    nome = str(input("Digite o nome do produto: "))
    quantidade = int(input("Quantidade do produto: "))
    lista_produtos.append(nome)
    lista_quantidade.append(quantidade)
    salvar_no_banco(conexao, nome, quantidade)
    print(f"{nome} registrado no banco de dados!")
    
def adicionar():
    if not lista_produtos:
        print("O estoque está vazio.")
        return
    print(f"Produtos atuais {lista_produtos}")
    nome = str(input("Digite o nome do produto para adicionar:\n"))
    if nome in lista_produtos:
        indice = lista_produtos.index(nome) 
        try:       
            qtd = int(input("Quantidade a adicionar: "))
            if qtd < 0:
                print("Erro: Digite um valor valido.")
            else:
                lista_quantidade[indice] += qtd
                print(f"{nome} adicionado com sucesso. {lista_quantidade[indice]}")               
        except ValueError:
            print('Erro! Digite apenas números')
    else:
        print("Produto inexistente.")
def retirar():
    if not lista_produtos:
        print("O estoque está vazio.")
        return
    print(f"Produtos disponiveis {lista_produtos}")
    nome = str(input("Digite o nome do produto para retirar:\n"))
    if nome in lista_produtos:
        indice = lista_produtos.index(nome)
        try:
            qtd = int(input("Quantidade a retirar: "))
            if qtd < 0:
                print("Erro: Digite um valor valido.")
            elif qtd > lista_quantidade[indice]:
                print(f"Erro: Tem apenas {lista_quantidade[indice]} em estoque.")
            else:           
                lista_quantidade[indice] -= qtd
                print(f"Quantidade de {nome} retirada com sucesso.")
        except ValueError:
            print("Valor invalido.")        
    else:
        print("Produto não encontrado.")    
def verificar():
    dicionario = dict(zip(lista_produtos, lista_quantidade))
    while True:
          try:
              verificacao = int(input("\nEscolha o que quer fazer:\n"
                                      "1_Verificar produtos\n"
                                      "2_Voltar"))
          except:
                print("Digite apenas números, por favor.")
                continue
          match verificacao:
                case 1:
                    print("\n---Produtos---\n")
                    for produto, quantidade in dicionario.items():
                        print(f"{produto}: {quantidade}")
                case 2:
                    break
                
                case _:
                    print("Opção inválida.")
                    
#Menu subalgoritmo:

def menu(conexao):
    while True:
        try:
            escolha = int(input("\nSelecione uma das opções:\n"
                                "1_Cadastro de Produtos\n" 
                                "2_Adicionar aos Produtos existentes\n"
                                "3_Retirar dos produtos existentes\n"
                                "4_Verificar produtos\n"
                                "5_Encerrar sistema\n"))
        except: 
            print("Digite apenas números, por favor.")
            continue
        match escolha:
            case 1:
                cadastrar(conexao)
            case 2:
                adicionar()
            case 3:
                retirar()
            case 4:
                verificar()
            case 5:
                print("Encerrando o sistema")
                break
            case _:
                print("Opção inválida")


# ---- Execução Principal ----

# Testando a conexão 
teste = conectar()
if teste:
    print("Conexão OK!")  

# Chamando o criador de tabela
criar_tabela(teste)                 

# Inicia o menu
print("===Bem-vindo ao gerenciador de estoques agrônomos===")
menu(teste)


