System.SetLength(CommandsP^,iCount+1);
CommandsP^[iCount].Operation:=Operation;
CommandsP^[iCount].Useage:=Usage;
CommandsP^[iCount].DataType:=DataType;
CommandsP^[iCount].PropertyID:=PropertyID;
CommandsP^[iCount].Value:=@Value;
CommandsP^[iCount].SQLPreOperator:=PreOp;
CommandsP^[iCount].SQLOperator:=Op;
Inc(iCount);
