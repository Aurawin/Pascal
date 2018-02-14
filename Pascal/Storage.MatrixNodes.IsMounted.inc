{$ifdef Unix}
  {$i Storage.MatrixNodes.IsMounted.Unix.inc}
{$else ifdefined(Windows)}
  {$i Storage.MatrixNodes.IsMounted.Windows.inc}
{$endif}

