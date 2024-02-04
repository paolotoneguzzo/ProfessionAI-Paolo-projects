import os
import json
from  shop_storage import _Storage




def number_check(numb,numb_conv):
    
    """
    It enables to: 
    Firstly, control if a string is convertible into an integer or a float type;
    Secondly, depending on the future use of the converted number, it checks if it is not a negative number 
    or if it is bigger than zero 
    
    Positional arguments:
    numb (str) -- the string to be converted and checked 
    numb_type (str) -- a string indicating what will represent the converted object (quantity or price)
    """
    
    if numb_conv == "quantity":
        
        try:
            number = int(numb)
            assert(number >= 0) 
            return number
        except AssertionError:
            return "Il parametro 'quantità' non può consistere di un numero negativo"
        except:
            return f"Il valore '{numb}' non è convertibile in intero"
        
    elif numb_conv == "price":
        
        try:
            number = float(numb)
            assert(number > 0) 
            return number
        except AssertionError:
            return "Il parametro 'prezzo' deve consistere di un numero positivo"
        except:
            return f"Il valore '{numb}' non è convertibile in decimale"


        
        
if __name__== "__main__":
    
    """
    It allows the managment of a vegan grocery shop. Specifically, it can:
    
    -Add new products within the shop's store;
    -List the products already present in the shop;
    -Register the selling performed;
    -Show the gross and net profit;
    -Help the user through a menu containing the possible commands.
    """
    
    if "VeganStore.json"in os.listdir():
        
        with open("VeganStore.json","a+", encoding = "utf-8") as json_file:
            json_file.seek(0)
            vegan_store = json.load(json_file) 
    else:
        vegan_store= {}
        
    storage = _Storage(vegan_store)
    help_string = "\nI comandi disponibili sono i seguenti:\naggiungi: aggiungi un prodotto al magazzino\nelenca:  elenca i prodotti in magazzino\nvendita: registra una vendita effettuata\nprofitti: mostra i profitti totali\naiuto: mostra i possibili comandi\nchiudi: esci dal programma "

    
    
    while True:
        command = input("\nInserisci un comando: ")
        
        if " " in command:
            command = command.replace(" ","")
        
        try: 
            assert(command in ["aggiungi","elenca","vendita","profitti","aiuto","chiudi"]), f"Il comando {command} non è permesso"
        except AssertionError as e:
            print(e)
            print(help_string)
            continue
            
        if command == "aggiungi":
            name = input("Nome del prodotto: ")
            amount = input("Quantità: ")
            quantity = number_check(amount,"quantity")
            
            if isinstance(quantity, str):
                print(quantity)
                continue
            else:
                if storage.prod_check(name) == True:
                    storage.prod_quick_add(name,quantity)
                    continue
                else:
                    buy = input("Prezzo d'acquisto: ")
                    buy_price = number_check(buy,"price")
                    
                    if isinstance(buy_price, str):
                        print(buy_price)
                        continue
                    else:
                        sell = input("Prezzo di vendita: ")
                        sell_price = number_check(sell,"price")
                        
                        if isinstance(sell_price, str):
                            print(sell_price)
                            continue 
                        else:
                            storage.prod_add(name, quantity, buy_price, sell_price) 
    
    
        elif command == "elenca":
            print("PRODOTTO QUANTITA'PREZZO\n")
            storage.prod_list() 
            
              
        elif command == "vendita": 
            sell_dict = {}
            
            while True:
                name = input("Nome del prodotto: ")
            
                try:
                    assert(storage.prod_check(name) == True), f"Errore: Il prodotto {name} non è presente in magazzino"
                except AssertionError as a:
                    print(a)
                    continue
                    
                amount =input("Quantità: ")
                sell_quantity = number_check(amount,"quantity")
                
                if isinstance(sell_quantity, str):
                    print(sell_quantity)
                    continue
                else:
                    sell_dict[name] = sell_quantity
                    question = input("Aggiungere un'altro prodotto?(sì/no): ")
                
                    try:
                        assert(question in ["si","sì","no"]), f"Errore: La risposta {question} non è valida"
                    except AssertionError as a:
                        print(a)
                        break
                
                    if question == "no":
                        break
                    elif question == "sì" or "si":
                        continue
                
            storage.prod_sell(sell_dict)
                   
                
        elif command == "profitti": 
            storage.get_profit()
              
                
        elif command == "aiuto":
            print(help_string)
              
                
        elif command == "chiudi":             
            with open("VeganStore.json","w+", encoding = "utf-8") as json_file:
                vegan_shop_str = json.dumps(vegan_store)
                json_file.write(vegan_shop_str)
            print("Bye bye")
            break