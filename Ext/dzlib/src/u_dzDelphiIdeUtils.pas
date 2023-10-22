unit u_dzDelphiIdeUtils;

interface

type
  DelphiVersionEnum = (dveD5, dveD6, dveD7,
    dveD2005, dveD2006, dveD2007,
    dveD2009, dveD2010,
    dveDXE, dveDXE2, dveDXE3, dveDXE4, dveDXE5, dveDXE6, dveDXE7, dveDXE8,
    dveD10, dveD101, dveD102, dveD103, dveD104,
    dveD11);

type
  TDelphiStringArr = array[DelphiVersionEnum] of string;
const
  LIBRARY_KEYS: TDelphiStringArr = (
    'Borland\Delphi\5.0',
    'Borland\Delphi\6.0',
    'Borland\Delphi\7.0',
    'Borland\BDS\3.0',
    'Borland\BDS\4.0',
    'Borland\BDS\5.0',
    'Codegear\BDS\6.0',
    'Codegear\BDS\7.0',
    'Embarcadero\BDS\8.0',
    'Embarcadero\BDS\9.0',
    'Embarcadero\BDS\10.0',
    'Embarcadero\BDS\11.0',
    'Embarcadero\BDS\12.0',
    'Embarcadero\BDS\14.0',
    'Embarcadero\BDS\15.0',
    'Embarcadero\BDS\16.0',
    'Embarcadero\BDS\17.0',
    'Embarcadero\BDS\18.0',
    'Embarcadero\BDS\19.0',
    'Embarcadero\BDS\20.0',
    'Embarcadero\BDS\21.0',
    'Embarcadero\BDS\22.0');

  DELPHI_NAMES: TDelphiStringArr = (
    '5', '6', '7',
    '2005', '2006', '2007',
    '2009', '2010',
    'XE', 'XE2', 'XE3', 'XE4', 'XE5', 'XE6', 'XE7', 'XE8',
    '10.0', '10.1', '10.2', '10.3', '10.4', '11');

implementation

end.
