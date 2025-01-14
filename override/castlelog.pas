unit CastleLog;

interface

procedure WritelnLog(const Category: string; const Message: string); overload;
procedure WritelnLog(const Message: string); overload;
procedure WritelnLog(const Category: string; const MessageBase: string; const Args: array of const); overload;
procedure WritelnLog(const MessageBase: string; const Args: array of const); overload;
procedure WritelnWarning(const Category: string; const Message: string); overload;
procedure WritelnWarning(const Message: string); overload;
procedure WritelnWarning(const Category: string; const MessageBase: string; const Args: array of const); overload;
procedure WritelnWarning(const MessageBase: string; const Args: array of const); overload;
implementation

procedure WritelnLog(const Category: string; const Message: string);
begin

end;

procedure WritelnLog(const Message: string);
begin

end;

procedure WritelnLog(const Category: string; const MessageBase: string; const Args: array of const);
begin

end;

procedure WritelnLog(const MessageBase: string; const Args: array of const);
begin

end;

procedure WritelnWarning(const Category: string; const Message: string);
begin

end;

procedure WritelnWarning(const Message: string);
begin

end;

procedure WritelnWarning(const Category: string; const MessageBase: string; const Args: array of const);
begin

end;

procedure WritelnWarning(const MessageBase: string; const Args: array of const);
begin

end;

end.
