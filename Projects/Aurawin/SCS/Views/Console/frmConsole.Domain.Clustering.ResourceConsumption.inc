pbDCRTotal.Position:=0;
pbDCRTotal.Max:=Storage.MatrixNodes.Node.DB.Count(Storage.Main.Task);
pbDCRTotal.Position:=pbDCRTotal.Max;
lblDCMRCTotal.Caption:=IntToStr(pbDCRTotal.Position);

pbDCRUsed.Position:=0;
pbDCRUsed.Max:=pbDCRTotal.Max;
pbDCRUsed.Position:=Storage.MatrixNodes.Node.DB.CountAllocated(Storage.Main.Task);
lblDCMRCUsed.Caption:=IntToStr(pbDCRUsed.Position);

pbDCRAvailable.Position:=0;
pbDCRAvailable.Max:=pbDCRTotal.Max;
pbDCRAvailable.Position:=Storage.MatrixNodes.Node.DB.CountAvailable(Storage.Main.Task);
lblDCMRCAvailable.Caption:=IntToStr(pbDCRAvailable.Position);
