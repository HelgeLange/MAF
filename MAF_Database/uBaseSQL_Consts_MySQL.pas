unit uBaseSQL_Consts_MySQL;

interface

const sQUERY_TEMPLATE_ID   : String = 'SELECT * FROM %Ident_DataStorageTable% WHERE DataID=:DataID';
      sQUERY_TEMPLATE_NAME : String = 'SELECT * FROM %Ident_DataStorageTable% WHERE Data_Category=:Data_Category AND Data_Name=:Data_Name';
      sUPDATE_TEMPLATE     : String = 'UPDATE %Ident_DataStorageTable% SET Data_Storage=:Data_Storage WHERE DataID=:DataID';
      sINSERT_TEMPLATE     : String = 'INSERT INTO %Ident_DataStorageTable% (Data_Category, Data_Name, Data_Storage) VALUES (:Data_Category, :Data_Name, :Data_Storage)';
      sBaseQueryUser        = 'SELECT A.ID, A.LOGIN, A.GROUPID, A.ACCOUNT_DATA, B.SL '+
                              'FROM %Ident_UserTable% A LEFT OUTER JOIN %Ident_GroupTable% B ON (A.GROUPID = B.ID) ';

      sQUERY_USERS_NAME    : String = sBaseQueryUser + 'WHERE A.Login LIKE :Login';
      sQUERY_USERS_ID      : String = sBaseQueryUser + 'WHERE A.ID=:ID';
      sUPDATE_USERS        : String = 'UPDATE %Ident_UserTable% SET GroupID=:GroupID, Account_Data=:Account_Data WHERE ID=:ID';
      sINSERT_USERS        : String = 'INSERT INTO %Ident_UserTable% (Login, GroupID, Account_Data) VALUES (:Login, :GroupID, :Account_Data)';

implementation

end.
