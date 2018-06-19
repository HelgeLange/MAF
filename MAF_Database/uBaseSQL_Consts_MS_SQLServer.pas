{*******************************************************************************
Name         : uBaseSQL_Consts_MS_SQLServer.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007-2009 by Helge Lange
Info         : HelgeLange@gmail.com
Date         : 23.05.2007
Last Update  : 23.05.2007
Version      : 1.0.001
Purpose      : constant definition file for MS SQL Server queries
               only changes for MS-SQL Server to the default file are here
Last Changes :

1.0.001 (23.05.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uBaseSQL_Consts_MS_SQLServer;

interface

const // default SQL for Templates
      sCREATE_TEMPLATE_TABLE : String = 'CREATE TABLE %Ident_TableName% (DataID int NOT NULL, ' +
                                        'Data_Category varchar(30), ' +
                                        'Data_Name varchar(30), ' +
                                        'Data_Storage image NULL)';

      // default SQL for Users
      sCREATE_USER_TABLE   : String = 'CREATE TABLE %Ident_TableName% (ID int NOT NULL,' +
                                      'Login varchar(30),'+
                                      'GroupID int'+
                                      'Account_Data image)';

implementation

end.
 