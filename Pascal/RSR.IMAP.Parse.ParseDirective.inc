fromString(Input[Index],ArgNext);
Inc(Index);
if ArgNext in SEARCH_SINGLE_ARG then begin
  ParseSingleTerm(ItemP,ArgNext);
end else if ArgNext in SEARCH_DOUBLE_ARG then begin
  ParseDoubleTerm(ItemP,ArgNext);
end else if ArgNext in SEARCH_TRIPPLE_ARG then begin
  ParseDoubleTerm(ItemP,ArgNext);
end;
