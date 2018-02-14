{
 unit Storage.Security.pas

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit Storage.Security;


interface

uses

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.Timer,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.Boolean,
  Core.Strings,

  Classes,
  SysUtils;

Const

  TI_FILTER_REFRESH               = 60*3;
  SEC_CONNECTION_SERVER_TTL       = 100800; // about a month
  SEC_VIOLATOR_SERVER_TTL         = 1;      // Years;
  SEC_WHITELIST_SERVER_TTL        = 1;      // Years;
  SEC_BLACKLIST_SERVER_TTL        = 1;      // Years;
  SEC_ACCEPTABLE_SERVER_TTL       = 1;      // Years;
  SEC_MAX_FILTER_THREADS          = 1;
  SEC_ELEVATE_AND_IDENTIFY        = 100;
  TopLevelRoots:array[0..519] of Core.Strings.VarString = (
    'com.ly',
    'net.ly',
    'gov.ly',
    'plc.ly',
    'edu.ly',
    'sch.ly',
    'med.ly',
    'org.ly',
    'id.ly',
    'ac.il',
    'co.il',
    'org.il',
    'net.il',
    'k12.il',
    'gov.il',
    'muni.il',
    'idf.il',
    'com.pk',
    'net.pk',
    'edu.pk',
    'org.pk',
    'fam.pk',
    'biz.pk',
    'web.pk',
    'gov.pk',
    'gok.pk',
    'gob.pk',
    'gkp.pk',
    'gop.pk',
    'gos.pk',
    'gog.pk',
    'ac.ru',
    'com.ru',
    'edu.ru',
    'gov.ru',
    'int.ru',
    'mil.ru',
    'net.ru',
    'org.ru',
    'pp.ru',
    'adygeya.ru',
    'bashkiria.ru',
    'buryatia.ru',
    'ulan-ude.ru',
    'grozny.ru',
    'cap.ru',
    'dagestan.ru',
    'nalchik.ru',
    'kalmykia.ru',
    'kchr.ru',
    'karelia.ru',
    'ptz.ru',
    'khakassia.ru',
    'komi.ru',
    'mari-el.ru',
    'mari.ru',
    'joshkar-ola.ru',
    'mordovia.ru',
    'yakutia.ru',
    'vladikavkaz.ru',
    'kazan.ru',
    'tatarstan.ru',
    'tuva.ru',
    'izhevsk.ru',
    'udmurtia.ru',
    'udm.ru',
    'altai.ru',
    'kamchatka.ru',
    'khabarovsk.ru',
    'khv.ru',
    'kuban.ru',
    'krasnoyarsk.ru',
    'perm.ru',
    'marine.ru',
    'vladivostok.ru',
    'stavropol.ru',
    'stv.ru',
    'chita.ru',
    'amur.ru',
    'arkhangelsk.ru',
    'astrakhan.ru',
    'belgorod.ru',
    'bryansk.ru',
    'chelyabinsk.ru',
    'chel.ru',
    'ivanovo.ru',
    'irkutsk.ru',
    'koenig.ru',
    'kaluga.ru',
    'kemerovo.ru',
    'kirov.ru',
    'vyatka.ru',
    'kostroma.ru',
    'kurgan.ru',
    'kursk.ru',
    'lipetsk.ru',
    'magadan.ru',
    'mosreg.ru',
    'murmansk.ru',
    'nnov.ru',
    'nov.ru',
    'novosibirsk.ru',
    'nsk.ru',
    'omsk.ru',
    'orenburg.ru',
    'oryol.ru',
    'penza.ru',
    'pskov.ru',
    'rnd.ru',
    'ryazan.ru',
    'samara.ru',
    'saratov.ru',
    'sakhalin.ru',
    'yuzhno-sakhalinsk.ru',
    'e-burg.ru',
    'yekaterinburg.ru',
    'smolensk.ru',
    'tambov.ru',
    'tver.ru',
    'tomsk.ru',
    'tom.ru',
    'tsk.ru',
    'tula.ru',
    'tyumen.ru',
    'simbirsk.ru',
    'vladimir.ru',
    'volgograd.ru',
    'tsaritsyn.ru',
    'vologda.ru',
    'cbg.ru',
    'voronezh.ru',
    'vrn.ru',
    'yaroslavl.ru',
    'mos.ru',
    'msk.ru',
    'spb.ru',
    'bir.ru',
    'jar.ru',
    'chukotka.ru',
    'surgut.ru',
    'yamal.ru',
    'amursk.ru',
    'baikal.ru',
    'cmw.ru',
    'fareast.ru',
    'jamal.ru',
    'kms.ru',
    'k-uralsk.ru',
    'kustanai.ru',
    'kuzbass.ru',
    'magnitka.ru',
    'mytis.ru',
    'nakhodka.ru',
    'nkz.ru',
    'norilsk.ru',
    'snz.ru',
    'oskol.ru',
    'pyatigorsk.ru',
    'rubtsovsk.ru',
    'syzran.ru',
    'tlt.ru',
    'vdonsk.ru',
    'zgrad.ru',
    'com.ph',
    'net.ph',
    'org.ph',
    'mil.ph',
    'gov.ph',
    'edu.ph',
    'com.cl',
    'net.cl',
    'org.cl',
    'gob.cl',
    'mil.cl',
    'idu.cl',
    'com.lb',
    'edu.lb',
    'gov.lb',
    'net.lb',
    'org.lb',
    'com.mk',
    'org.mk',
    'net.mk',
    'edu.mk',
    'gov.mk',
    'inf.mk',
    'name.mk',
    'pro.mk',
    'com.gt',
    'edu.gt',
    'net.gt',
    'gob.gt',
    'org.gt',
    'mil.gt',
    'ind.gt',
    'com.ae',
    'net.ae',
    'gov.ae',
    'mil.ae',
    'org.ae',
    'com.pl',
    'biz.pl',
    'net.pl',
    'art.pl',
    'edu.pl',
    'org.pl',
    'ngo.pl',
    'gov.pl',
    'info.pl',
    'mil.pl',
    'bialystok.pl',
    'bydgoszcz.pl',
    'czest.pl',
    'elk.pl',
    'gda.pl,',
    'gorzow.pl',
    'kalisz.pl',
    'katowice.pl',
    'konin.pl',
    'kraków.pl',
    'lodz.pl',
    'lublin.pl',
    'malopolska.pl',
    'nysa.pl',
    'olsztyn.pl',
    'opole.pl',
    'pila.pl',
    'poznan.pl',
    'radom.pl',
    'rzeszow.pl',
    'slupsk.pl',
    'szczecin.pl',
    'slask.pl',
    'tychy.pl',
    'torun.pl',
    'wroc.pl',
    'wroclaw.pl',
    'waw.pl',
    'warszawa.pl',
    'zgora.pl',
    'com.uy',
    'edu.uy',
    'gub.uy',
    'net.uy',
    'mil.uy',
    'org.uy',
    'edu.pe',
    'gob.pe',
    'nom.pe',
    'mil.pe',
    'sld.pe',
    'org.pe',
    'com.pe',
    'net.pe',
    'com.hk',
    'org.hk',
    'net.hk',
    'edu.hk',
    'gov.hk',
    'idv.hk',
    'com.ar',
    'edu.ar',
    'gob.ar',
    'gov.ar',
    'int.ar',
    'mil.ar',
    'net.ar',
    'org.ar',
    'tur.ar',
    'argentina.ar',
    'congresodelalengua3.ar',
    'educ.ar',
    'gobiernoelectronico.ar',
    'mecon.ar',
    'nacion.ar',
    'nic.ar',
    'promocion.ar',
    'retina.ar',
    'uba.ar',
    'ac.cn',
    'com.cn',
    'edu.cn',
    'gov.cn',
    'mil.cn',
    'net.cn',
    'org.cn',
    'ah.cn',
    'bj.cn',
    'cq.cn',
    'fj.cn',
    'gd.cn',
    'gs.cn',
    'gz.cn',
    'gx.cn',
    'ha.cn',
    'hb.cn',
    'he.cn',
    'hi.cn',
    'hk.cn',
    'hl.cn',
    'hn.cn',
    'jl.cn',
    'js.cn',
    'jx.cn',
    'ln.cn',
    'mo.cn',
    'nm.cn',
    'nx.cn',
    'qh.cn',
    'sc.cn',
    'sd.cn',
    'sh.cn',
    'sn.cn',
    'sx.cn',
    'tj.cn',
    'tw.cn',
    'xj.cn',
    'xz.cn',
    'yn.cn',
    'zj.cn',
    'com.co',
    'org.co',
    'edu.co',
    'gov.co',
    'net.co',
    'mil.co',
    'nom.co',
    'com.ua',
    'edu.ua',
    'gov.ua',
    'net.ua',
    'in.ua',
    'at.ua',
    'pp.ua',
    'org.ua',
    'cherkassy.ua',
    'chernigov.ua',
    'chernovtsy.ua',
    'crimea.ua',
    'dnepropetrovsk.ua',
    'donetsk.ua',
    'ivano-frankivsk.ua',
    'kharkov.ua',
    'kherson.ua',
    'khmelnitskiy.ua',
    'kiev.ua',
    'kirovograd.ua',
    'lugansk.ua',
    'lutsk.ua',
    'lviv.ua',
    'nikolaev.ua',
    'odessa.ua',
    'poltava.ua',
    'rovno.ua',
    'sebastopol.ua',
    'yalta.ua',
    'sumy.ua',
    'ternopil.ua',
    'uzhgorod.ua',
    'vinnica.ua',
    'zaporizhzhe.ua',
    'zhitomir.ua',
    'net.co',
    'ac.uk',
    'co.uk',
    'gov.uk',
    'judiciary.uk',
    'ltd.uk',
    'me.uk',
    'mod.uk',
    'net.uk',
    'nhs.uk',
    'nic.uk',
    'org.uk',
    'parliament.uk',
    'plc.uk',
    'police.uk',
    'sch.uk',
    'com.au',
    'net.au',
    'org.au',
    'gov.au',
    'edu.au',
    'co.nz',
    'net.nz',
    'org.nz',
    'govt.nz',
    'ac.jp',
    'ad.jp',
    'co.jp',
    'ed.jp',
    'go.jp',
    'gr.jp',
    'lg.jp',
    'ne.jp',
    'or.jp',
    'com.tw',
    'net.tw',
    'org.tw',
    'co.in',
    'edu.in',
    'org.in',
    'net.in',
    'gov.in',
    'ac.za',
    'city.za',
    'co.za',
    'edu.za',
    'gov.za',
    'law.za',
    'mil.za',
    'nom.za',
    'org.za',
    'school.za',
    'alt.za',
    'work.za',
    'ngo.za',
    'tm.za',
    'web.za',
    'com.sg',
    'net.sg',
    'org.sg',
    'gov.sg',
    'edu.sg',
    'per.sg',
    'idn.sg',
    'com.my',
    'com.mx',
    'net.mx',
    'org.mx',
    'edu.mx',
    'gob.mx',
    'adm.br',
    'adv.br',
    'agr.br',
    'am.br',
    'arq.br',
    'art.br',
    'ato.br',
    'b.br',
    'bio.br',
    'blog.br',
    'bmd.br',
    'cim.br',
    'cng.br',
    'cnt.br',
    'com.br',
    'coop.br',
    'ecn.br',
    'edu.br',
    'eng.br',
    'esp.br',
    'etc.br',
    'eti.br',
    'far.br',
    'flog.br',
    'fm.br',
    'fnd.br',
    'fot.br',
    'fst.br',
    'g12.br',
    'ggf.br',
    'gov.br',
    'imb.br',
    'ind.br',
    'inf.br',
    'jor.br',
    'jus.br',
    'lel.br',
    'mat.br',
    'med.br',
    'mil.br',
    'mus.br',
    'net.br',
    'nom.br',
    'not.br',
    'ntr.br',
    'odo.br',
    'org.br',
    'ppg.br',
    'pro.br',
    'psc.br',
    'psi.br',
    'qsl.br',
    'radio.br',
    'rec.br',
    'slg.br',
    'srv.br',
    'taxi.br',
    'teo.br',
    'tmp.br',
    'trd.br',
    'tur.br',
    'tv.br',
    'vet.br',
    'vlog.br',
    'wiki.br',
    'zlg.br',
    'com.tr',
    'gen.tr',
    'org.tr',
    'biz.tr',
    'info.tr',
    'av.tr',
    'dr.tr',
    'bel.tr',
    'tsk.tr',
    'bbs.tr',
    'k12.tr',
    'edu.tr',
    'name.tr',
    'net.tr',
    'gov.tr',
    'pol.tr',
    'web.tr',
    'tel.tr',
    'tv.tr',
    'nc.tr',
    'nic.tr',
    'gov.nc.tr'
  );
Type
  // TDBSecurityKind.  You can only add values
  // To modify just set space holder like secReserved

  TDBSecurityKind=(
    // !!!WARNING!!!
    // MUST APPEND TO THIS LIST
    // DO NOT CHANGE ORDER
    // VALUES ARE PERMANANT
    secContentFilter,   // exact match prohibit
    secBlackList,       // domain name black list
    secWhiteList,       // domain name white list
    secContentProfiles, // index of prohibited content must match all in profile
    secWLService,       // domain name of dns white list service
    secBLService,       // domain name of dns black list service
    secConnections,     // domain names of every server that made a connection with server
    secAcceptableList,  // domain name is acceptable but subject to security policy
    secTopLevelDomains, // domain name of internet top levels
    secViolatorIP,      // ip address of security rule violation
    secReserved1,
    secReserved2,
    secReserved3,
    secReserved4,
    secReserved5,
    secReserved6,
    secReserved7,
    secReserved8,
    secReserved9,
    secNone
  );
  TDBSecurityKinds=set of TDBSecurityKind;
const
  FILTER_INC_WHITES:TDBSecurityKinds=[secConnections,secWhiteList];
  FILTER_NON_WHITES:TDBSecurityKinds=[secContentFilter,secWhiteList,secContentProfiles,secWLService,secBLService];
  FILTER_INC_BLACKS:TDBSecurityKinds=[secContentFilter,secBlackList];
  FILTER_NON_BLACKS:TDBSecurityKinds=[secContentFilter,secBlackList,secContentProfiles,secWLService,secBLService];
  FILTER_INC_ACCEPTABLE:TDBSecurityKinds=[secConnections,secAcceptableList];
  FILTER_NON_ACCEPTABLE:TDBSecurityKinds=[secContentFilter,secContentProfiles,secWLService,secBLService];
  FILTER_INC_LISTED:TDBSecurityKinds=[secConnections,secWhiteList,secBlackList,secAcceptableList];
  FILTER_NON_LISTED:TDBSecurityKinds=[secContentFilter,secWhiteList,secBlackList,secAcceptableList];
  FILTER_INC_TOPLEVEL:TDBSecurityKinds=[secTopLevelDomains];
  FILTER_NON_TOPLEVEL:TDBSecurityKinds=[secContentFilter,secWhiteList,secBlackList,secAcceptableList,secTopLevelDomains];

  Kind_Value:Array[TDBSecurityKind] of Integer=(
    // **** APPEND ONLY ****
    0, // secContentFilter,   // exact match prohibit
    1, // secBlackList,       // domain name black list
    2, // secWhiteList,       // domain name white list
    3, // secContentProfiles, // index of prohibited content must match all in profile
    4, // secWLService,       // domain name of dns white list service
    5, // secBLService,       // domain name of dns black list service
    6, // secConnections,     // domain names of every server that made a connection with server
    7, // secAcceptableList   // domain name is acceptable but subject to security policy
    8, // secViolatorIP       // ip address of security rules violator
    9, // Reserved
    10,// Reserved
    11,// Reserved
    12,// Reserved
    13,// Reserved
    14,// Reserved
    15,// Reserved
    16,// Reserved
    17,// Reserved
    18,// Reserved
    19 // None
  );
Type
  Filter=Class
  Type
    Item=record
      ID                         : Core.Database.Types.LargeWord;
      Counter                    : Core.Database.Types.LargeWord;
      Stale                      : Core.Database.Types.Bool;
      Value                      : Core.Database.Types.VarString;
      Expires                    : Core.Database.Types.Double;
      Kind                       : TDBSecurityKind;
      Enabled                    : Core.Database.Types.Bool;
      Data                       : Core.Arrays.Types.VarString; // runtime
    end;
    PItem=^Item;
    Items=Array of PItem;
    PItems=^Items;

    Manifest=Array[TDBSecurityKind] of Items;
    DB=Class
    Type
      IDs=Class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        Counter                  : Core.Database.Types.Integer = 2;
        Value                    : Core.Database.Types.Integer = 3;
        Expires                  : Core.Database.Types.Integer = 4;
        Kind                     : Core.Database.Types.Integer = 5;
        Enabled                  : Core.Database.Types.Integer = 6;
        Data                     : Core.Database.Types.Integer = 7;
        Stale                    : Core.Database.Types.Integer = 8;
        InKind                   : Core.Database.Types.Integer = 9;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        Counter                  : Core.Database.Types.VarString = 'ITMCTR';
        Value                    : Core.Database.Types.VarString = 'ITMVAL';
        Expires                  : Core.Database.Types.VarString = 'ITMEXP';
        Kind                     : Core.Database.Types.VarString = 'ITMKND';
        Enabled                  : Core.Database.Types.VarString = 'ITMEND';
        Data                     : Core.Database.Types.VarString = 'ITMDAT';
        Stale                    : Core.Database.Types.VarString = 'ITMSTL';
        InKind                   : Core.Database.Types.VarString = 'INKIND';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
      AutoCreate           : True;
      AutoCommit           : True;
      Group                : 'Security';
      Name                 : 'Data Filters';
      Value                : 'scs_sec_fltrs';
      Hint                 : 'Storage of data for use in security policy enforcement';
      PrimaryKeyP          : @Keys.ID;
      );
      Fields: array [0..9] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Counter; KeyP: @Keys.Counter; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Value; KeyP: @Keys.Value; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Expires; KeyP: @Keys.Expires; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Enabled; KeyP: @Keys.Enabled; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftString; AutoCreate: false; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Stale; KeyP: @Keys.Stale; DataType: dftBoolean; AutoCreate: false; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.InKind; KeyP: @Keys.InKind; DataType: dftString; AutoCreate: false; Verified: False; Precision: 0; Flags: cfNone;  )
      );

      class function Violator_Delist(Task:Core.Database.Types.TTask; Item:Core.Strings.VarString):boolean;
      class function Blacklist_Delist(Task:Core.Database.Types.TTask; Item:Core.Strings.VarString):boolean;

      class function Fill(Task:Core.Database.Types.TTask; Var Entries:Manifest):boolean; overload;
      class function Live(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Var Entries:Items):boolean;
      class function Find(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Term:Core.Strings.VarString; Var Entries:Items):boolean; overload;
      class function Find(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind;  Const inModes,exModes:TDBSecurityKinds; Term:Core.Strings.VarString; dtMin,dtMax:Double; Var Entries:Items):boolean; overload;
      class function Find(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind;  Term:Core.Strings.VarString; dtMax:Double; Var Entries:Items):boolean; overload;

      class function Fill(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Var Entries:Items):boolean; overload;
      class function Clear(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entries:Items):boolean; overload;
      class function Clear(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind):boolean; overload;

      class function Delete(Task:Core.Database.Types.TTask; Var Entry:Item):boolean;
      class function Edit(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
      class function Add(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
      class function Increment(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item; Count:QWord=1):boolean;
      class function SetExpires(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;

      class function Identify(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
      class function Elevate(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;

      class function Exists(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; var Data:Core.Strings.VarString):boolean; overload;
      class function Exists(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean; overload;

      class function Stale(Task:Core.Database.Types.TTask; ID:QWord):boolean; overload;
      class function Stale(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Value:Core.Strings.VarString):boolean; overload;
      class function Stale(Task:Core.Database.Types.TTask; const Modes:TDBSecurityKinds; Value:Core.Strings.VarString):boolean; overload;

      class function VerifyDefaultTopLevelDomains(Task:Core.Database.Types.TTask):boolean;
    end;

    class procedure Invalidate(Var Entries:Items);
    class procedure Duplicate(Var Source,Dest:Manifest);overload;
    class procedure Duplicate(Var Source,Dest:Items);overload;
    class procedure Copy(Var Source,Dest:Items);overload;
    class procedure Copy(Var Source,Dest:Item);overload;
    class procedure SetSize(Var Entries:Items; Size:LongInt);
    class function  Push(EntryP:PItem; var Entries:Items): LongInt;

    class procedure Empty(Var Entry:Item); overload;

    class procedure Empty(Var Entries:Items); overload;

    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:Items); overload;
    class procedure Done(Var Entries:Manifest); overload;

    class procedure Init(Var Entry:Item); overload;
    class procedure Init(Var Entries:Items); overload;
    class procedure Init(Var Entries:Manifest); overload;

    class procedure toString(Var Source:Items; Var Destination:Core.Arrays.Types.VarString); overload;
    class procedure toString(var Source:Items; var Destination:Core.Arrays.Types.StringManifest); overload;
    class procedure setData(var Source:Items); overload;

    class function  getItem(var Entries:Items; ID:QWord):PItem;
    class function  getTopLevel(var Entry:Core.Strings.VarString):Core.Strings.VarString;
    class procedure Cleanup(var Entries:Items);

    class function IsTopLevelDomainListed(Entry:Core.Strings.VarString; var Entries:Items; out ID:QWord):boolean;


  end;
  function Prepare(Value:Core.Strings.VarString):Core.Strings.VarString;

implementation
uses
  db,
  DateUtils,
  StrUtils;

procedure cbDestroyFilters(ItemP:Core.Database.Monitor.Types.PItem);
begin
  Core.Database.Done(Filter.DB.TableP^);
  Filter.DB.TableP:=nil;
  Filter.DB.MonitorP:=nil;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with Filter.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyFilters, Core.Database.Monitor.Notify.None);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

class function Filter.getTopLevel(var Entry:Core.Strings.VarString):Core.Strings.VarString;
var
  iLcv:LongInt;
  iLen,iCt,jLcv:LongInt;
  bHadAlpha:boolean;
begin
  iLen:=System.Length(Entry);
  iLcv:=iLen;
  iCt:=0;
  bHadAlpha:=false;
  SetLength(Result,0);
  While (iLcv>0) do begin
    bHadAlpha:=bHadAlpha or not (Entry[iLcv] in ['.','0','1','2','3','4','5','6','7','8','9']);
    If (Entry[iLcv]='.') then begin
      Inc(iCt);
      if (iCt>1) then begin
        for jLcv:=0 to High(TopLevelRoots) do begin
          if (Result=TopLevelRoots[jLcv]) then begin
            Dec(iCt);
            break;
          end;
        end;
      end;
    end;
    if (iCt<2) then
      Result:=Concat(Entry[iLcv],Result);
    Dec(iLcv);
  end;
  if (bHadAlpha=false) then
    Result:=Entry;
end;

class function Filter.IsTopLevelDomainListed(Entry:Core.Strings.VarString; var Entries:Items; out ID:QWord):boolean;
var
  iLcv:LongInt;
begin
  Result:=false;
  for iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]^.Enabled) and (Entries[iLcv]^.Value=Entry) then begin
      Result:=True;
      ID:=Entries[iLcv]^.ID;
      Break;
    end;
  end;
end;

procedure CB_Security_List_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP                          : Filter.PItems;
  itmP                           : Filter.PItem;
  iCount                         : LongInt;
  iID                            : QWord;
begin
  ListP:=DataP;
  iCount:=System.Length(ListP^);
  iID:=Fields.FieldByName(Filter.DB.Keys.ID).AsLargeInt;
  itmP:=Filter.getItem(ListP^,iID);
  if itmP=nil then begin
    new(itmP);
    Filter.Init(itmP^);
    itmP^.ID:=iID;
    SetLength(ListP^,iCount+1);
    ListP^[iCount]:=itmP;
  end;
  itmP^.Stale:=false;
  itmP^.Counter:=Fields.FieldByName(Filter.DB.Keys.Counter).AsLargeInt;
  itmP^.Value:=Fields.FieldByName(Filter.DB.Keys.Value).AsString;
  itmP^.Enabled:=Fields.FieldByName(Filter.DB.Keys.Enabled).AsBoolean;
  itmP^.Expires:=Fields.FieldByName(Filter.DB.Keys.Expires).AsFloat;
  itmP^.Kind:=TDBSecurityKind(Fields.FieldByName(Filter.DB.Keys.Kind).AsInteger);

end;

procedure CB_Security_List_Fill_Item(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmP                           : Filter.PItem;
begin
  itmP:=DataP;
  itmP^.ID:=Fields.FieldByName(Filter.DB.Keys.ID).AsLargeInt;
  itmP^.Counter:=Fields.FieldByName(Filter.DB.Keys.Counter).AsLargeInt;
  itmP^.Stale:=false;
  itmP^.Value:=Fields.FieldByName(Filter.DB.Keys.Value).AsString;
  itmP^.Enabled:=Fields.FieldByName(Filter.DB.Keys.Enabled).AsBoolean;
  itmP^.Expires:=Fields.FieldByName(Filter.DB.Keys.Expires).AsFloat;
  itmP^.Kind:=TDBSecurityKind(Fields.FieldByName(Filter.DB.Keys.Kind).AsInteger);
end;


class function Filter.DB.Fill(Task:Core.Database.Types.TTask; Var Entries:Manifest):boolean;
begin
  Result:=true;
  Fill(Task,secContentFilter,Entries[secContentFilter]);
  Fill(Task,secBlackList,Entries[secBlackList]);
  Fill(Task,secWhiteList,Entries[secWhiteList]);
  Fill(Task,secContentProfiles,Entries[secContentProfiles]);

  Fill(Task,secWLService,Entries[secWLService]);
  Fill(Task,secBLService,Entries[secBLService]);

  Fill(Task,secConnections,Entries[secConnections]);
end;

class function Filter.DB.VerifyDefaultTopLevelDomains(Task:Core.Database.Types.TTask):boolean;
var
  iLcv:LongInt;
  Entry:Item;
begin
  Init(Entry);
  Try
    Entry.Kind:=secTopLevelDomains;
    Entry.Counter:=0;
    for iLcv:=0 to High(TopLevelRoots) do begin
      Entry.Value:=TopLevelRoots[iLcv];
      if Exists(Task,secTopLevelDomains,Entry)=false then
        Add(Task,secTopLevelDomains,Entry);
    end;
    Result:=true;
  Finally
    Done(Entry);
  end;
end;

class function Filter.DB.Fill(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Var Entries:Items):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode); Invalidate(Entries);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForOrderBy, IDS.ID,poNone,oDescending,Commands);
    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
  Cleanup(Entries);
end;

class function Filter.DB.Identify(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode); Entry.Stale:=true;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poAnd,oEqual,Entry.Value,Commands);
    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill_Item,@Entry);
    if (Entry.Stale=true) then begin
      Entry.Counter:=1;
      Entry.Enabled:=True;
      case Mode of
        secConnections : begin
          Entry.Expires:=DateUtils.IncSecond(Core.Timer.dtUT,SEC_CONNECTION_SERVER_TTL);
        end;
        secBlackList : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_BLACKLIST_SERVER_TTL);
        end;
        secWhiteList : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_WHITELIST_SERVER_TTL);
        end;
        secViolatorIP : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_VIOLATOR_SERVER_TTL);
        end;
      end;
      Result:=Add(Task,Mode,Entry);
    end else begin
      case Mode of
        secConnections : begin
          Entry.Expires:=DateUtils.IncSecond(Core.Timer.dtUT,SEC_CONNECTION_SERVER_TTL);
          Increment(Task,Mode,Entry);
          SetExpires(Task,Mode,Entry);
        end;
        secBlackList : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_BLACKLIST_SERVER_TTL);
          Increment(Task,Mode,Entry);
        end;
        secWhiteList : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_WHITELIST_SERVER_TTL);
          Increment(Task,Mode,Entry);
          SetExpires(Task,Mode,Entry);
        end;
        secViolatorIP : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_VIOLATOR_SERVER_TTL);
          Increment(Task,Mode,Entry);
          SetExpires(Task,Mode,Entry);
        end;
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Filter.DB.Elevate(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode); Entry.Stale:=true;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poAnd,oEqual,Entry.Value,Commands);
    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill_Item,@Entry);
    if (Entry.Stale=true) then begin
      Entry.Counter:=SEC_ELEVATE_AND_IDENTIFY;
      Entry.Enabled:=True;
      case Mode of
        secConnections : begin
          Entry.Expires:=DateUtils.IncSecond(Core.Timer.dtUT,SEC_CONNECTION_SERVER_TTL);
        end;
        secBlackList : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_BLACKLIST_SERVER_TTL);
        end;
      end;
      Result:=Add(Task,Mode,Entry);
    end else begin
      case Mode of
        secConnections : begin
          Entry.Expires:=DateUtils.IncSecond(Core.Timer.dtUT,SEC_CONNECTION_SERVER_TTL);
          Increment(Task,Mode,Entry,SEC_ELEVATE_AND_IDENTIFY);
          SetExpires(Task,Mode,Entry);
        end;
        secBlackList : begin
          Entry.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_BLACKLIST_SERVER_TTL);
        end;
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Filter.DB.Exists(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; var Data:Core.Strings.VarString):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Count                          : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poAnd,oEqual,Data,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Expires,poAnd,oGreaterThan,Core.Timer.dtUT,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Filter.DB.Exists(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode); Entry.Stale:=true;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poAnd,oEqual,Entry.Value,Commands);

    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill_Item,@Entry) and (Entry.Stale=false);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Filter.DB.Stale(Task:Core.Database.Types.TTask; ID:QWord):boolean;
var
  iCount                         : LongInt;
  Count                          : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count=0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Filter.DB.Stale(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Value:Core.Strings.VarString):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Count                          : QWord;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poAnd,oEqual,Value,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count=0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Filter.DB.Stale(Task:Core.Database.Types.TTask; Const Modes:TDBSecurityKinds; Value:Core.Strings.VarString):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  kLcv                           : TDBSecurityKind;
  bFirst                         : boolean;
  Count                          : QWord;
begin
  Result:=False; iCount:=0; bFirst:=true;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poNone,oEqual,Value,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket, poAnd,oOpenBracket,Commands);

    for kLcv:=Low(Modes) to High(Modes) do begin
      if kLcv in Modes then begin
        if bFirst=true then begin
          Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,Kind_Value[kLcv],Commands);
          bFirst:=false;
        end else
          Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poOr,oEqual,Kind_Value[kLcv],Commands);
      end;
    end;
    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket, poNone,oCloseBracket,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Filter.DB.Live(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Var Entries:Items):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  dtUT                           : Double;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; dtUT:=Core.Timer.dtUT; iKind:=Integer(Mode); Invalidate(Entries);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Expires,poAnd,oGreaterThan,dtUT,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Enabled,poAnd,oEqual,Core.Arrays.Boolean.Yes,Commands);
    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
  Cleanup(Entries);
end;

class function Filter.DB.Find(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Term:Core.Strings.VarString; Var Entries:Items):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode); Invalidate(Entries); Term:=Core.Database.Like_Prep(Term);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poAnd,oLike,Term,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForOrderBy, IDS.ID,poNone,oDescending,Commands);
    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
  Cleanup(Entries);
end;


class function Filter.DB.Find(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Term:Core.Strings.VarString; dtMax:double; Var Entries:Items):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;

begin
  Result:=False;
  iCount:=0;
  iKind:=Integer(Mode);
  Invalidate(Entries);
  Term:=Core.Database.Like_Prep(Term);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poNone,oLike,Term,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poAnd,oEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Expires,poAnd,oLessThan,dtMax,Commands);
    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;

  Cleanup(Entries);
end;

class function Filter.DB.Find(Task:Core.Database.Types.TTask; Const Mode:TDBSecurityKind; Const inModes,exModes:TDBSecurityKinds; Term:Core.Strings.VarString; dtMin,dtMax:double; Var Entries:Items):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  kLcv                           : TDBSecurityKind;
  Commands                       : Core.Database.Types.Commands;
  ExFilterList                   : Items;

  function hasExclusion(var Value:Core.Strings.VarString; var Entries:Items; const exK:TDBSecurityKind):boolean;
  var
    iLcv:LongInt;
  begin
    Result:=False;
    for iLcv:=0 to High(Entries) do begin
      if (Entries[iLcv]^.Value=Value) then begin
        Result:=true;
        exit;
      end;
    end;
  end;

  procedure doExclusions(Entries,exItems:Items; const exK:TDBSecurityKind);
  var
    iLcv : LongInt;
  begin
    for iLcv:=0 to High(Entries) do
      if (Entries[iLcv]^.Stale=false) then
        Entries[iLcv]^.Stale:=hasExclusion(Entries[iLcv]^.Value,exItems,exK);
  end;

begin
  Result:=False;
  iCount:=0;
  iKind:=Integer(secNone);
  Invalidate(Entries);
  Term:=Core.Database.Like_Prep(Term);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Value,poNone,oLike,Term,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poAnd,oNotEqual,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Expires,poAnd,oLessThan,dtMax,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForOrderBy, IDS.Counter,poNone,oDescending,Commands);

    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Expires,poAnd,oGreaterThan,dtMin,Commands);
    {$i Storage.Security.Read.FilterItem.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Security_List_Fill,@Entries);
    if (exModes<>[]) then begin
      for kLcv:=Low(exModes) to High(exModes) do begin
        if kLcv in  exModes then begin
          Fill(Task,kLcv,ExFilterList);
          doExclusions(Entries,ExFilterList,kLcv);
        end;
      end;
    end;

  Finally
    Core.Database.Done(Commands);
  End;
  Done(ExFilterList);
  Cleanup(Entries);
end;

class function Filter.DB.Clear(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entries:Items):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0; iKind:=Integer(Mode);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    Empty(Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.Clear(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0; iKind:=Integer(Mode);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.Kind,poNone,oEqual,iKind,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.Violator_Delist(Task:Core.Database.Types.TTask; Item:Core.Strings.VarString):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  saIP                           : Core.Arrays.Types.VarString;
  Kind                           : LongInt;
begin
  iCount:=0;
  Kind:=Integer(secViolatorIP);
  Core.Arrays.VarString.Init(saIP);
  Try
    Core.Arrays.VarString.fromString(saIP,Item,'.');
    if Length(saIP)=4 then begin
      Item:=Concat(saIP[0],'.',saIP[1],'.',saIP[2],'.%');
    end;
  finally
    Core.Arrays.VarString.Done(saIP);
  end;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.Kind,poNone,oEqual,Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.Value,poAnd,oLike,Item,Commands);

    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.Blacklist_Delist(Task:Core.Database.Types.TTask; Item:Core.Strings.VarString):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Kind                           : LongInt;
begin
  iCount:=0;
  Kind:=Integer(secBlacklist);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.Kind,poNone,oEqual,Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.Value,poAnd,oEqual,Item,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Item):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0;
  Try
    Entry.Stale:=true;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.SetExpires(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDS.Expires,poNone,oNone,Entry.Expires,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.Increment(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item; Count:QWord=1):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP, useForIncrement, IDS.Counter, poNone, oNone, Count, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.Edit(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDS.Value,poNone,oNone,Entry.Value,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDS.Expires,poNone,oNone,Entry.Expires,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDS.Enabled,poNone,oNone,Entry.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDS.Counter,poNone,oNone,Entry.Counter,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Filter.DB.Add(Task:Core.Database.Types.TTask; const Mode:TDBSecurityKind; Var Entry:Item):boolean;
var
  iCount                         : LongInt;
  iInsertID                      : QWord;
  iReset                         : QWord;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iKind:=Integer(Mode);
  iInsertID:=Random(High(Integer)); iReset:=0;
  Try
    // Table
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Insert Primary ID with universal auto-number
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDS.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDS.InsertID,poNone,oNone,iReset,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDS.ID,poNone,oNone,Entry.ID,Commands);


    // Insert with these values
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDS.Kind,poNone,oNone,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDS.Value,poNone,oNone,Entry.Value,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDS.Expires,poNone,oNone,Entry.Expires,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDS.Counter,poNone,oNone,Entry.Counter,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDS.Enabled,poNone,oNone,Entry.Enabled,Commands);
    Result:= Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Filter.Empty(Var Entry:Item);
begin
  Entry.ID:=0;
  Entry.Expires:=0.0;
  Entry.Enabled:=true;
  Entry.Kind:=secNone;
  Core.Strings.Empty(Entry.Value);
  Core.Arrays.VarString.Empty(Entry.Data);
end;

class procedure Filter.Invalidate(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Entries[iLcv]^.Stale:=true;
end;

class procedure Filter.Duplicate(Var Source,Dest:Manifest);
var
  sLcv:TDBSecurityKind;
begin
  For sLcv:=Low(TDBSecurityKind) to High(TDBSecurityKind) do
    Duplicate(Source[sLcv],Dest[sLcv]);
end;

class procedure Filter.Duplicate(var Source,Dest:Items);
var
  iLen,iLcv:LongInt;
begin
  iLen:=System.Length(Source);
  SetLength(Dest,iLen);
  For iLcv:=0 to High(Source) do
    Dest[iLcv]:=Source[iLcv];
end;

class procedure Filter.SetSize(Var Entries:Items; Size:LongInt);
var
  iLcv,Len:LongInt;
  itmP:PItem;
begin
  Len:=System.Length(Entries);
  if Len<Size then begin
    // Grow List
    SetLength(Entries,Size);
    for iLcv:=Len to Size-1 do begin
      new(itmP);
      Init(itmP^);
      Entries[iLcv]:=itmP;
    end;
  end else if Len>Size then begin
    // Shrink List
    While (Len>Size) and (Len>0) do begin
      itmP:=Entries[Len-1];
      Done(itmP^);
      Dispose(itmP);
      dec(Len);
    end;
    SetLength(Entries,Len);
  end;
end;

class procedure Filter.Copy(var Source,Dest:Items);
var
  iLen,iLcv:LongInt;
  itmP:PItem;
begin
  iLen:=System.Length(Source);
  SetSize(Dest,iLen);
  for iLcv:=0 to High(Source) do
    Copy(Source[iLcv]^,Dest[iLcv]^);
end;

class procedure Filter.Copy(var Source,Dest:Item);
begin
  Dest.ID:=Source.ID;
  Dest.Stale:=Source.Stale;
  Dest.Value:=Source.Value;
  Dest.Counter:=Source.Counter;
  Dest.Expires:=Source.Expires;
  Dest.Enabled:=Source.Enabled;
  Dest.Kind:=Source.Kind;
  Core.Arrays.VarString.Copy(Source.Data,Dest.Data);
end;

class function  Filter.Push(EntryP:PItem; var Entries:Items): LongInt;
begin
  Result:=System.Length(Entries);
  System.SetLength(Entries,Result+1);
  Entries[Result]:=EntryP;
end;

class procedure Filter.Empty(Var Entries:Items);
var
  iLcv:LongInt;
  itmP:PItem;
begin
  for iLcv:=0 to High(Entries) do begin
    itmP:=Entries[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  SetLength(Entries,0);
end;

class procedure Filter.Init(Var Entry:Item);
begin
  Entry.ID:=0;
  Entry.Enabled:=true;
  Entry.Kind:=secNone;
  Entry.Stale:=false;
  Entry.Expires:=0;
  Entry.Counter:=0;
  SetLength(Entry.Value,0);
  Core.Arrays.VarString.Init(Entry.Data);
End;

class procedure Filter.Init(Var Entries:Items);
var
  iLcv:LongInt;
  itmP:PItem;
begin
  for iLcv:=0 to High(Entries) do begin
    itmP:=Entries[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  SetLength(Entries,0);
end;

class procedure Filter.Init(Var Entries:Manifest);
begin
  Empty(Entries[secContentFilter]);
  Empty(Entries[secBlackList]);
  Empty(Entries[secWhiteList]);
  Empty(Entries[secWhiteList]);
  Empty(Entries[secContentProfiles]);
End;

class procedure Filter.Done(Var Entry:Item);
begin
  Finalize(Entry.Value);
  Core.Arrays.VarString.Done(Entry.Data);
  Finalize(Entry);
End;

class procedure Filter.Done(Var Entries:Items);
var
  iLcv:LongInt;
  itmP:PItem;
begin
  for iLcv:=0 to High(Entries) do begin
    itmP:=Entries[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  Finalize(Entries);
End;

class procedure Filter.Done(Var Entries:Manifest);
begin
  Done(Entries[secContentFilter]);
  Done(Entries[secBlackList]);
  Done(Entries[secWhiteList]);
  Done(Entries[secContentProfiles]);
  Done(Entries);
End;

class procedure Filter.toString(Var Source:Items; Var Destination:Core.Arrays.Types.VarString);
var
  iLcv:LongInt;
begin
  Core.Arrays.VarString.Empty(Destination);
  for iLcv:=0 to High(Source) do begin
    if (Source[iLcv]^.Enabled=true) then
      Core.Arrays.VarString.Add(Destination,Source[iLcv]^.Value);
  end;
end;

class procedure Filter.toString(var Source:Items; var Destination:Core.Arrays.Types.StringManifest);
var
  iLcv:LongInt;
  iLength:LongInt;
  iBlock:LongInt;
  iBlockEnd:LongInt;
  iBlockLcv:LongInt;
  iBlockLen:LongInt;
  sBlock:Core.Strings.VarString;
  sEntry:Core.Strings.VarString;
  bCapture:boolean;
  saP:Core.Arrays.Types.PVarString;
begin
  Core.Arrays.VarString.Empty(Destination); iLength:=0;
  for iLcv:=0 to High(Source) do begin
    if (Source[iLcv]^.Enabled=true) then begin
      iBlock:=Pos('[',Source[iLcv]^.Value);
      if (iBlock>0) then begin
        SetLength(Destination,iLength+1);
        saP:=@Destination[iLength];
        sBlock:=Source[iLcv]^.Value;
        iBlockLen:=Length(sBlock);
        iBlockLcv:=1;
        bCapture:=false;
        SetLength(sEntry,0);
        While (iBlockLcv<iBlockLen) do begin
          bCapture:=bCapture or (sBlock[iBlockLcv]='[');
          if bCapture=false then begin
            if (sBlock[iBlockLcv] in [' ','[',']']) then begin
              // Regular word is over.  Add entry to list
              Core.Arrays.VarString.Add(saP^,sEntry,[aoCheckForDuplicates]);
              SetLength(sEntry,0);
            end else begin
              sEntry:=Concat(sEntry,sBlock[iBlockLcv]);
            end;
          end else begin
            if (sBlock[iBlockLcv] in ['[',']']) then begin
              // Capture is over.  Add entry to list
              Core.Arrays.VarString.Add(saP^,sEntry,[aoCheckForDuplicates]);
              SetLength(sEntry,0);
            end else begin
              sEntry:=Concat(sEntry,sBlock[iBlockLcv]);
            end;
          end;
          inc(iBlockLcv);
        end;
        iBlockLen:=Length(sEntry);
        if iBlockLen>0 then begin
          Core.Arrays.VarString.Add(saP^,sEntry,[aoCheckForDuplicates]);
          SetLength(sEntry,0);
        end;
        Inc(iLength);
      end else begin
        SetLength(Destination,iLength+1);
        fromString(Destination[iLength],Source[iLcv]^.Value,#32);
        Inc(iLength);
      end;
    end;
  end;
end;

class procedure Filter.setData(var Source:Items);
var
  iLcv:LongInt;
  iBlock:LongInt;
  iBlockEnd:LongInt;
  iBlockLcv:LongInt;
  iBlockLen:LongInt;
  sBlock:Core.Strings.VarString;
  sEntry:Core.Strings.VarString;
  bCapture:boolean;
  bCaptured:boolean;
  saP:Core.Arrays.Types.PVarString;
begin
  for iLcv:=0 to High(Source) do begin
    Core.Arrays.VarString.Empty(Source[iLcv]^.Data);
    if (Source[iLcv]^.Enabled=true) then begin
      iBlock:=Pos('[',Source[iLcv]^.Value);
      if (iBlock>0) then begin
        saP:=@Source[iLcv]^.Data;
        sBlock:=Source[iLcv]^.Value;
        iBlockLen:=Length(sBlock);
        iBlockLcv:=1;
        bCapture:=false;
        bCaptured:=false;
        SetLength(sEntry,0);
        While (iBlockLcv<=iBlockLen) do begin
          bCapture:=bCapture or (sBlock[iBlockLcv]='[');
          if bCapture=false then begin
            if (sBlock[iBlockLcv]=#32) then begin
              // Regular word is over.  Add entry to list
              if Length(sEntry)>0 then
                Core.Arrays.VarString.Add(saP^,sEntry,[aoCheckForDuplicates]);
              SetLength(sEntry,0);
              bCapture:=false;
              bCaptured:=false;
            end else begin
              sEntry:=Concat(sEntry,sBlock[iBlockLcv]);
            end;
          end else begin
            if (sBlock[iBlockLcv] in ['[',']']) then begin
              if (bCaptured=true) then begin
                // Capture is over.  Add entry to list
                if Length(sEntry)>0 then
                  Core.Arrays.VarString.Add(saP^,sEntry,[aoCheckForDuplicates]);
                SetLength(sEntry,0);
                bCapture:=false;
                bCaptured:=false;
              end else begin
                bCaptured:=true;
              end;
            end else begin
              sEntry:=Concat(sEntry,sBlock[iBlockLcv]);
            end;
          end;
          inc(iBlockLcv);
        end;
        if Length(sEntry)>0 then begin
          Core.Arrays.VarString.Add(saP^,sEntry,[aoCheckForDuplicates]);
          SetLength(sEntry,0);
        end;
      end else begin
        fromString(Source[iLcv]^.Data,Source[iLcv]^.Value,#32);
      end;
    end;
  end;
end;

class procedure Filter.Cleanup(var Entries:Items);
var
  nLcv,jlcv,iLcv:LongInt;
  itmP:PItem;
  iCount:LongInt;
begin
  iLcv:=0;
  iCount:=System.Length(Entries);
  while (iLcv<iCount) do begin
    itmP:=Entries[iLcv];
    if (itmP<>nil) and (itmP^.Stale=true) then begin
      Dec(iCount);
      Done(itmP^);
      Dispose(itmP);
      Entries[iLcv]:=nil;
      for jLcv:=iLcv to iCount-1 do
        Entries[jLcv]:=Entries[jLcv+1];
    end else
      inc(iLcv);
  end;
  if iCount<0 then iCount:=0;
  SetLength(Entries,iCount);
end;

class function  Filter.getItem(var Entries:Items; ID:QWord):PItem;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Entries) do begin
    if Entries[iLcv]^.ID=ID then begin
      Result:=Entries[iLcv];
      break;
    end;
  end;
end;

function Prepare(Value:Core.Strings.VarString):Core.Strings.VarString;
begin
  Value:=Lowercase(Value);
  Result:=UTF8Encode(Value);
end;

initialization
  RegisterDBM;

end.

