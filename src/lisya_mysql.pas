unit lisya_mysql;

{$mode delphi}{$H+}

{$ASERTION ON}

interface

uses
    Classes, SysUtils, mysql55conn, mysql50conn, sqldb, odbcconn, FileUtil,
    dlisp_values;

type
    {$DEFINE mysql55}
    { Tmodule_mysql }

    Tmodule_mysql = class(TDataModule)
        MySQL50: TMySQL50Connection;
        MySQL55: TMySQL55Connection;
        Query: TSQLQuery;
        SQLTransaction50: TSQLTransaction;
        SQLTransaction55: TSQLTransaction;
    private
        { private declarations }
    public
        { public declarations }
    end;


    { TVSQL }

    TVSQL = class(TValue)
        module: Tmodule_mysql;
        {$IfDef mysql50}
        connector: TMySQL50Connection;
        {$ENDIF}
        {$IfDef mysql55}
        connector: TMySQL55Connection;
        {$ENDIF}
        constructor CreateMySQL(
            database: unicodestring;
            host: unicodestring = 'localhost';
            port: integer = 3306;
            username: unicodestring =  '';
            password: unicodestring = '');
        destructor Destroy; override;

        function connection_parameters: unicodestring;
        procedure Print; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;
    end;


    { TVSQLPointer }

    TVSQLPointer = class(TValue)
        body: PVariable;
        query: TSQLQuery; //переменная для упрощения доступа к телу (не освобождать)
        constructor Create(P: PVariable);
        constructor CreateMySQL(
            database: unicodestring;
            host: unicodestring = 'localhost';
            port: integer = 3306;
            username: unicodestring =  '';
            password: unicodestring = '');
        destructor Destroy; override;

        procedure Print; override;
        function Copy: TValue; override;
        function AsString: unicodestring; override;

        procedure Commit;
    end;





var
    module_mysql: Tmodule_mysql;

implementation

{$R *.lfm}

{ TVSQLPointer }

constructor TVSQLPointer.Create(P: PVariable);
begin
    body := P;
    body.constant:=true;
    query := (body.V as TVSQL).module.Query;
end;

constructor TVSQLPointer.CreateMySQL(
    database: unicodestring; host: unicodestring; port: integer;
    username: unicodestring; password: unicodestring);
begin
    body := NewVariable;
    body.V := TVSQL.CreateMySQL(database,host,port,username,password);
    body.constant:=true;

    query := (body.V as TVSQL).module.Query;
end;

destructor TVSQLPointer.Destroy;
begin
    ReleaseVariable(body);
    inherited Destroy;
end;

procedure TVSQLPointer.Print;
begin
    WriteLn(AsString);
end;

function TVSQLPointer.Copy: TValue;
begin
    result := TVSQLPointer.Create(RefVariable(body));
end;

function TVSQLPointer.AsString: unicodestring;
begin
    result := '<#SQL '+ (body.V as TVSQL).connection_parameters+'>';
end;

procedure TVSQLPointer.Commit;
begin
    (body.V as TVSQL).module.SQLTransaction55.Commit;
end;

{ TVSQL }

constructor TVSQL.CreateMySQL(database: unicodestring;
    host: unicodestring; port: integer; username: unicodestring;
    password: unicodestring);
begin
    module := Tmodule_mysql.Create(nil);
    {$IfDef mysql50}
    connector := module.MySQL50;
    {$ENDIF}
    {$IfDef mysql55}
    connector := module.MySQL55;
    {$ENDIF}
    connector.DatabaseName:=database;
    connector.HostName:=host;
    connector.Port:=port;
    connector.UserName:=username;
    connector.Password:=password;
    if (username<>'') and (password<>'') then connector.LoginPrompt:=true;
    module.Query.DataBase := connector;
    connector.Connected:=true;

    //костыль
    module.Query.SQL.Text:='set character_set_connection=utf8';
    module.Query.ExecSQL;
end;

destructor TVSQL.Destroy;
begin
    module.Free;
    inherited Destroy;
end;

function TVSQL.connection_parameters: unicodestring;
begin
    result := connector.UserName+'@'+connector.DatabaseName+':'+connector.HostName;
end;

procedure TVSQL.Print;
begin
    WriteLn(AsString);
end;

function TVSQL.Copy: TValue;
begin
    Assert(true,'копирование TVSQL');
end;

function TVSQL.AsString: unicodestring;
begin
    result := '#<SQL CONNECTION '+connection_parameters+'>';
end;


end.

