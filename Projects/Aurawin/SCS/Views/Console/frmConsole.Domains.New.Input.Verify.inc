  btnDomainsNew_OK.Enabled:=(
    (Length(txtDomains_Hostname.Text)>0) and
    (Length(txtDomains_FriendlyName.Text)>0) and
    (Length(txtDomains_Postmaster.Text)>0)
  );
