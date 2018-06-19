unit uBaseSQL_Consts_Accuracer;

interface

const sQUERY_TEMPLATE_ID   : String = 'SELECT * FROM %Ident_DataStorageTable% WHERE DataID=:DataID';
      sQUERY_TEMPLATE_NAME : String = 'SELECT * FROM %Ident_DataStorageTable% WHERE Data_Category=:Data_Category AND Data_Name=:Data_Name';
      sUPDATE_TEMPLATE     : String = 'UPDATE %Ident_DataStorageTable% SET Data_Storage=:Data_Storage WHERE DataID=:DataID';
      sINSERT_TEMPLATE     : String = 'INSERT INTO %Ident_DataStorageTable% (Data_Category, Data_Name, Data_Storage) VALUES (:Data_Category, :Data_Name, :Data_Storage)';

implementation

end.
