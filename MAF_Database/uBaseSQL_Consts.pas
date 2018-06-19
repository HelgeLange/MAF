{*******************************************************************************
Name         : uBaseSQL_Consts.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2009 by Helge Lange
Info         : HelgeLange@gmail.com
Date         : 13.05.2007
Last Update  : 30.09.2009
Version      : 1.0.001
Purpose      : constant definition file for Standard SQL 
Last Changes :

1.0.000 (30.09.2009) -----------------------------------------------------------
- [DEL] deleted unused SQLs
1.0.000 (13.06.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uBaseSQL_Consts;

interface

const sUserTableIdent         : String = '%Ident_UserTable%';
      sGroupTableIdent        : String = '%Ident_GroupTable%';
      sDataStorageTableIdent  : String = '%Ident_DataStorageTable%';
      sUserSecurityTableIdent : String = '%Ident_UserSecurityTable%';

      sCREATE_TEMPLATE_TABLE : String = 'CREATE TABLE %Ident_DataStorageTable% (DataID int NOT NULL, ' +
                                        'Data_Category varchar(30), ' +
                                        'Data_Name varchar(30), ' +
                                        'Data_Storage image NULL)';

      sQUERY_TEMPLATE_ID   : String = 'SELECT * FROM %Ident_DataStorageTable% WHERE DataID=:DataID';
      sQUERY_TEMPLATE_NAME : String = 'SELECT * FROM %Ident_DataStorageTable% WHERE Data_Category=:Data_Category AND Data_Name=:Data_Name';
      sUPDATE_TEMPLATE     : String = 'UPDATE %Ident_DataStorageTable% SET Data_Storage=:Data_Storage WHERE DataID=:DataID';
      sINSERT_TEMPLATE     : String = 'INSERT INTO %Ident_DataStorageTable% (Data_Category, Data_Name, Data_Storage) VALUES (:Data_Category, :Data_Name, :Data_Storage)';
      sDELETE_TEMPLATE     : String = 'DELETE FROM %Ident_DataStorageTable% WHERE DataID=:DataID';

      sQUERY_USERSECUITY   : String = 'SELECT * FROM %Ident_UserSecurityTable% WHERE Data_Type=:Data_Type';
      sINSERT_USERSECURITY : String = 'INSERT INTO %Ident_UserSecurityTable% (Data_Type, Data_Stream) VALUES (:Data_Type, :Data_Stream)';
      sUPDATE_USERSECURITY : String = 'UPDATE %Ident_UserSecurityTable% SET Data_Stream=:Data_Stream WHERE Data_Type=:Data_Type';

      // default SQL for Users
      sCREATE_USER_TABLE   : String = 'CREATE TABLE %Ident_UserTable% (ID int NOT NULL,' +
                                      'Login varchar(30),'+
                                      'GroupID int,'+
                                      'Account_Data image)';
      sBaseQueryUser        = 'SELECT A.ID, A.LOGIN, A.GROUPID, A.ACCOUNT_DATA, B.SL '+
                              'FROM %Ident_UserTable% A LEFT OUTER JOIN %Ident_GroupTable% B ON (A.GROUPID = B.ID) ';

      sQUERY_USERS_NAME    : String = sBaseQueryUser + 'WHERE A.Login=:Login';
      sQUERY_USERS_ID      : String = sBaseQueryUser + 'WHERE A.ID=:ID';
      sUPDATE_USERS        : String = 'UPDATE %Ident_UserTable% SET GroupID=:GroupID, Account_Data=:Account_Data WHERE ID=:ID';
      sINSERT_USERS        : String = 'INSERT INTO %Ident_UserTable% (Login, GroupID, Account_Data) VALUES (:Login, :GroupID, :Account_Data)';
      sDELETE_USERS        : String = 'DELETE FROM %Ident_UserTable% WHERE ID=:ID';
      sLIST_ALL_USERS      : String = 'SELECT * FROM %Ident_UserTable% ORDER BY Login';
      sLIST_USER_BY_GROUP  : String = 'SELECT * FROM %Ident_UserTable% WHERE GroupID=:GroupID ORDER BY Login';

      sCREATE_GROUP_TABLE  : String = 'CREATE TABLE %Ident_GroupTable% (ID INTEGER NOT NULL,' +
                                      'NAME VARCHAR(100) CHARACTER SET NONE NOT NULL COLLATE NONE)';
      sQUERY_GROUPS        : String = 'SELECT * FROM %Ident_GroupTable% ORDER BY Name';
      sINSERT_GROUP        : String = 'INSERT INTO %Ident_GroupTable% (Name, SL) VALUES (:Name, :SL)';
      sDELETE_GROUP_ID     : String = 'DELETE FROM %Ident_GroupTable% WHERE ID=:ID';
      sUPDATE_GROUP_ID     : String = 'UPDATE %Ident_GroupTable% SET Name=:Name, SL=:SL WHERE ID=:ID';
      sSELECT_BY_GROUP_ID  : String = 'SELECT * FROM %Ident_GroupTable% WHERE ID=:ID';

      ID_QUERY_TEMPLATE_NAME          = 1;
      ID_SELECT_RORDER_HOOKS          = 2;
      ID_UPDATE_HOOK_ORDER            = 3;

implementation

end.
 