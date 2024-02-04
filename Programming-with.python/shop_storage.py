class _Storage:
        
        """
        It allows the managing of a vegan grocery shop and the related accounting
        """
        
        
        def __init__(self,shop_dict):
            
            """
            Positional arguments:        
            shop_dict (dict) -- a dictonary containing the shop's products
            """
            
            self._shop_dict = shop_dict  
         
        
        def __getitem__(self,keys):
            
            """
            It performs the indexing of a product contained in the object's istance
            
            Positional arguments:
            keys (str) -- index (key) to have access to the respective 'value'
            """
            
            value = self._shop_dict[keys]
            return value
        
        
        def prod_check(self,name):
            
            """
            It verifies the presence of a product within the shop's storage
            
            Positional arguments:
            name (str) -- the name of the putative product
            """
            
            if name in self._shop_dict:
                return True
                   
                
        def prod_add(self, name, quantity, buy_price, sell_price):
            
            """ 
            It adds a product within the shop's storage
            
            Positional arguments:
            name (str) -- the name of the putative product
            quantity (int) -- the quantity in terms of units or packages 
            buy_price (float) -- the buying price per unit/package 
            sell_price (float) -- the selling price per unit/package 
            """
            
            self._shop_dict[name] = {}
            self._shop_dict[name]["Quantità"] = quantity
            self._shop_dict[name]["Prezzo di acquisto"] = buy_price
            self._shop_dict[name]["Prezzo di vendita"] = sell_price
            return print(f"\nAGGIUNTO:{quantity} X {name}")
        
        
        def prod_quick_add(self,name,quantity):
            
            """
            It allows a further addition of a product that currently is, or previously was, contained within the storage
            
            Positional arguments:
            name (str) -- the name of the putative product
            quantity (int) -- the quantity in terms of units or packages
            """
            
            if name in self._shop_dict:
                   self._shop_dict[name]["Quantità"] = self._shop_dict[name]["Quantità"] + quantity 
            return print(f"\nAGGIUNTO:{quantity} X {name}")
                  
            
        def prod_list(self):
            
            """
            It lists the products of the shop 
            """        
            
            prod_list = str()
            for name in self._shop_dict:
                prod_list += f"{name}, {self._shop_dict[name]['Quantità']}, €{self._shop_dict[name]['Prezzo di vendita']}\n" 
            return print(prod_list)    
        

        def prod_sell(self, sell_dict): 
            
            """
            It enables the selling of the shop's products 
            
            Positional arguments:
            sell_dict (dict) -- the dictionary containing names and units/packages of products being sold
            """
            
            money_earned = float()
            prod_sell = str()
            for name in sell_dict:
                if sell_dict[name] > self._shop_dict[name]["Quantità"]:
                    prod_sell += f"Il prodotto {name} non è presente nella quantità specificata\n"
                else:
                    prod_sell += f"{sell_dict[name]} X {name}: €{self._shop_dict[name]['Prezzo di vendita']:.2f}\n"
                    self._shop_dict[name]["Quantità"] = self._shop_dict[name]["Quantità"] - sell_dict[name]
                    money_earned += self._shop_dict[name]['Prezzo di vendita']*sell_dict[name] 
                    if "Profitto lordo" and "Profitto netto" in self._shop_dict[name]:
                        self._shop_dict[name]["Profitto lordo"] += self._shop_dict[name]['Prezzo di vendita']*sell_dict[name]
                        self._shop_dict[name]["Profitto netto"] +=(self._shop_dict[name]['Prezzo di vendita']-self._shop_dict[name]['Prezzo di acquisto'])*sell_dict[name]
                    else:
                        self._shop_dict[name]["Profitto lordo"] = self._shop_dict[name]['Prezzo di vendita']*sell_dict[name]
                        self._shop_dict[name]["Profitto netto"] =(self._shop_dict[name]['Prezzo di vendita']-self._shop_dict[name]['Prezzo di acquisto'])*sell_dict[name]    
            return print(f"\nVENDITA REGISTRATA\n{prod_sell}\nTOTALE: €{money_earned:.2f}" if money_earned != 0 else f"{prod_sell}")
        
        
        def get_profit(self): 
            
            """
            It prints the gross and net profit of the shop    
            """
            
            gross_prof = float()
            net_prof = float()
            for name in self._shop_dict:
                if "Profitto lordo" and "Profitto netto" in self._shop_dict[name]:
                    gross_prof += self._shop_dict[name]["Profitto lordo"]
                    net_prof += self._shop_dict[name]["Profitto netto"]
            return print(f"Profitto: lordo=€{gross_prof:.2f} netto=€{net_prof:.2f}")       