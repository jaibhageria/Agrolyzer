def NNFriendly():
    import pandas as pd
    import numpy as np
    
    Data = pd.read_csv("TrimmedAgro.csv")
    
    #Data Set Transorm
    NewData = pd.DataFrame()
    NewData['Production']=Data['Production']
    NewData['Area']=Data['Area']
    NewData['Crop']=Data['Crop']
    NewData['District_Name']=Data['District_Name']
    
    
    LUQCrop = NewData["Crop"].unique()
    LUQDname= NewData["District_Name"].unique()
    
    
    LUQCrop1  = [(LUQCrop[i],i+1) for i in range(len(LUQCrop))]
    
    LUQDname1  = [(LUQDname[i],i+1) for i in range(len(LUQDname))]
    
    CropDictionary={Key:Value for Key,Value in LUQCrop1}
    
    DNameDictionary={Key:Value for Key,Value in LUQDname1}
    
    
        
    Lappend = pd.Series([CropDictionary[Value] for Value in NewData["Crop"]])
    DAppend = pd.Series([DNameDictionary[Value] for Value in NewData["District_Name"]])
    
    NewData["Crop"]=Lappend
    NewData["District_Name"]=DAppend
    
    NewData.to_csv("NNF.csv")
    
