unit u_dzCompilerAndRtlVersions;

{$INCLUDE 'dzlib.inc'}

interface

const
  CompilerVersionDelphi6 = 14;
  CompilerVersionDelphi7 = 15;
  CompilerVersionDelphi8 = 16;
  // naming scheme changed to year numbers (curse you Microsoft!)
  CompilerVersionDelphi2005 = 17;
  CompilerVersionDelphi2006 = 18;
  CompilerVersionDelphi2007 = 18.5;
  CompilerVersionDelphi2007Net = 19;
  CompilerVersionDelphi2009 = 20;
  CompilerVersionDelphi2010 = 21;
  // naming scheme changed to XEn
  CompilerVersionDelphiXE = 22;
  CompilerVersionDelphiXE2 = 23;
  CompilerVersionDelphiXE3 = 24;
  CompilerVersionDelphiXE4 = 25;
  // Did anybody ever use AppMethod?
  CompilerVersionAppMethod = 26.5;
  CompilerVersionDelphiXE5 = 26;
  CompilerVersionDelphiXE6 = 27;
  CompilerVersionDelphiXE7 = 28;
  CompilerVersionDelphiXE8 = 29;
  // Naming scheme changed to 10.n
  CompilerVersionDelphiX100 = 30;
  CompilerVersionDelphiX101 = 31;
  CompilerVersionDelphiX102 = 32;
  CompilerVersionDelphiX103 = 33;
  CompilerVersionDelphiX104 = 34;
  CompilerVersionDelphiX110 = 35;
{$IF CompilerVersion > CompilerVersionDelphiX110}
{$MESSAGE HINT 'Add new CompilerVersion(s) here'}
{$IFEND}

  // if you are better at memorizing city names
  CompilerVersionDelphiSeattle = 30;
  CompilerVersionDelphiBerlin = 31;
  CompilerVersionDelphiTokyo = 32;
  CompilerVersionDelphiRio = 33;
  CompilerVersionDelphiSydney = 34;
  CompilerVersionDelphiAlexandria = 35;

{$IF CompilerVersion > CompilerVersionDelphiAlexandria}
{$MESSAGE HINT 'Add new CompilerVersion(s) here'}
{$IFEND}

const
  RtlVersionDelphi6 = 14;
  RtlVersionDelphi7 = 15;
  RtlVersionDelphi8 = 16;
  // naming scheme changed to year numbers
  RtlVersionDelphi2005 = 17;
  RtlVersionDelphi2006 = 18;
  RtlVersionDelphi2007 = 18; // did not change!
  RtlVersionDelphi2007Net = 19;
  RtlVersionDelphi2009 = 20;
  RtlVersionDelphi2010 = 21;
  // naming scheme changed to XEn
  RtlVersionDelphiXE = 22;
  RtlVersionDelphiXE2 = 23;
  RtlVersionDelphiXE3 = 24;
  RtlVersionDelphiXE4 = 25;
  RtlVersionDelphiXE5 = 26;
  RtlVersionDelphiXE6 = 27;
  RtlVersionDelphiXE7 = 28;
  RtlVersionDelphiXE8 = 29;
  // Naming scheme changed to 10.n
  RtlVersionDelphiX100 = 30;
  RtlVersionDelphiX101 = 31;
  RtlVersionDelphiX102 = 32;
  RtlVersionDelphiX103 = 33;
  RtlVersionDelphiX104 = 34;
  RtlVersionDelphiX110 = 35;
{$IF RtlVersion > RtlVersionDelphiX110}
{$MESSAGE HINT 'Add new RtlVersion(s) here'}
{$IFEND}

  // if you are better at memorizing city names
  RtlVersionDelphiSeattle = 30;
  RtlVersionDelphiBerlin = 31;
  RtlVersionDelphiTokyo = 32;
  RtlVersionDelphiRio = 33;
  RtlVersionDelphiSydney = 34;
  RtlVersionDelphiAlexandria = 35;

{$IF RtlVersion > RtlVersionDelphiAlexandria}
{$MESSAGE HINT 'Add new RtlVersion(s) here'}
{$IFEND}

implementation

end.
