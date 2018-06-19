{*******************************************************************************
Name         : uBaseSQL_Consts_Firebird.pas
Coding by    : Helge Lange
Copyright    : (c)opyright 2007 by Helge Lange
Info         : HelgeLange@gmail.com
Date         : 20.03.2007
Last Update  : 20.03.2007
Version      : 1.0.001
Purpose      :
Last Changes :

1.0.001 (20.03.2007) -----------------------------------------------------------
- initial version
*******************************************************************************}
unit uBaseSQL_Consts_Firebird;

interface

const // default SQL for Templates
      sCREATE_TEMPLATE_TABLE : String = 'CREATE TABLE %Ident_TableName% (TemplateID int NOT NULL, ' +
                                        'Category varchar(30), ' +
                                        'Template_Name varchar(30), ' +
                                        'Template_Data blob)';

      // default SQL for Users
      sCREATE_USER_TABLE   : String = 'CREATE TABLE %Ident_TableName% ('+
                                      'ID INTEGER NOT NULL,' +
                                      'Login varchar(30),'+
                                      'GroupID INTEGER,'+
                                      'FirstName varchar(100),'+
                                      'LastName varchar(100),'+
                                      'Password varchar(255),'+
                                      'Password2 varchar(255),'+
                                      'Flags INTEGER DEFAULT 0 NOT NULL)';
implementation

end.
