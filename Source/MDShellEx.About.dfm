object FrmAbout: TFrmAbout
  Left = 651
  Top = 323
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 316
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    446
    316)
  PixelsPerInch = 96
  TextHeight = 13
  object TitleLabel: TLabel
    Left = 190
    Top = 8
    Width = 187
    Height = 41
    AutoSize = False
    Caption = 'Markdown Shell Extensions'
    WordWrap = True
  end
  object LabelVersion: TLabel
    Left = 328
    Top = 64
    Width = 35
    Height = 13
    Caption = 'Version'
  end
  object Panel1: TPanel
    Left = 0
    Top = 265
    Width = 446
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object btnOK: TButton
      Left = 360
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnIssues: TButton
      Left = 8
      Top = 16
      Width = 125
      Height = 25
      Caption = 'Submit issue...'
      ImageIndex = 0
      TabOrder = 0
      OnClick = btnIssuesClick
    end
    object btnCheckUpdates: TButton
      Left = 139
      Top = 16
      Width = 125
      Height = 25
      Caption = 'Check for updates'
      ImageIndex = 3
      TabOrder = 1
      Visible = False
      OnClick = btnCheckUpdatesClick
    end
  end
  object MemoCopyRights: TMemo
    Left = 11
    Top = 87
    Width = 427
    Height = 172
    Anchors = [akLeft, akTop, akBottom]
    Color = clBtnFace
    Lines.Strings = (
      'Author:'
      'Carlo Barazzetta'
      'https://github.com/EtheaDev/MarkdownShellExtensions'
      'Copyright '#169' 2021 all rights reserved.'
      ''
      'Other libraries from Ethea:'
      'SVGIconImageList'
      'https://github.com/EtheaDev/SVGIconImageList/'
      ''
      'Third parties libraries:'
      'OpenSLL Library: Cryptography and SSL/TLS Toolkit'
      'Copyright '#169' 1998-2018 The OpenSSL Project.  All rights reserved.'
      ''
      'Delphi Markdown'
      'https://github.com/grahamegrieve/delphi-markdown'
      
        'Copyright (c) 2011+, Health Intersections Pty Ltd All rights res' +
        'erved.'
      ''
      'Delphi Preview Handler'
      'https://github.com/RRUZ/delphi-preview-handler'
      'The Initial Developer of the Original Code is Rodrigo Ruz V.'
      
        'Portions created by Rodrigo Ruz V. are Copyright '#169' 2011-2021 Rod' +
        'rigo Ruz V.'
      ''
      'SynEdit http://synedit.svn.sourceforge.net/viewvc/synedit/'
      'all rights reserved.'
      ''
      'Synopse/SynPDF https://github.com/synopse/SynPDF'
      'Copyright '#169' Synopse: all right reserved.'
      ''
      'HtmlToPdf https://github.com/MuzioValerio/HtmlToPdf'
      'Copyright '#169' Muzio Valerio.'
      ''
      
        '- TSVG Library - http://www.mwcs.de Original version '#169' 2005, 200' +
        '8 Martin '
      'Walter.'
      ''
      'HTMLViewer - https://github.com/BerndGabriel/HtmlViewer'
      'Copyright (c) 1995 - 2008 by L. David Baldwin'
      'Copyright (c) 1995 - 2008 by Anders Melander (DitherUnit.pas)'
      'Copyright (c) 1995 - 2008 by Ron Collins (HtmlGif1.pas)'
      'Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)'
      'Copyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)'
      'Copyright (c) 2009 - 2019 by HtmlViewer Team')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object LinkLabel1: TLinkLabel
    Left = 8
    Top = 62
    Width = 303
    Height = 19
    Caption = 
      '<a href="https://github.com/EtheaDev/MarkdownShellExtensions">ht' +
      'tps://github.com/EtheaDev/MarkdownShellExtensions</a>'
    TabOrder = 2
    UseVisualStyle = True
    OnClick = LinkLabel1Click
  end
  object SVGIconImage1: TSVGIconImage
    Left = 383
    Top = 8
    Width = 52
    Height = 49
    AutoSize = False
    SVGText = 
      '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'#13#10'<svg'#13#10'  ' +
      ' xmlns:dc="http://purl.org/dc/elements/1.1/"'#13#10'   xmlns:cc="http:' +
      '//creativecommons.org/ns#"'#13#10'   xmlns:rdf="http://www.w3.org/1999' +
      '/02/22-rdf-syntax-ns#"'#13#10'   xmlns:svg="http://www.w3.org/2000/svg' +
      '"'#13#10'   xmlns="http://www.w3.org/2000/svg"'#13#10'   xmlns:sodipodi="htt' +
      'p://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"'#13#10'   xmlns:inksc' +
      'ape="http://www.inkscape.org/namespaces/inkscape"'#13#10'   width="100' +
      '"'#13#10'   height="100"'#13#10'   version="1.1"'#13#10'   id="svg58"'#13#10'   sodipodi' +
      ':docname="logo.svg">'#13#10'  <metadata'#13#10'     id="metadata62">'#13#10'    <r' +
      'df:RDF>'#13#10'      <cc:Work'#13#10'         rdf:about="">'#13#10'        <dc:for' +
      'mat>image/svg+xml</dc:format>'#13#10'        <dc:type'#13#10'           rdf:' +
      'resource="http://purl.org/dc/dcmitype/StillImage" />'#13#10'        <d' +
      'c:title></dc:title>'#13#10'      </cc:Work>'#13#10'    </rdf:RDF>'#13#10'  </metad' +
      'ata>'#13#10'  <sodipodi:namedview'#13#10'     pagecolor="#ffffff"'#13#10'     bord' +
      'ercolor="#666666"'#13#10'     borderopacity="1"'#13#10'     objecttolerance=' +
      '"10"'#13#10'     gridtolerance="10"'#13#10'     guidetolerance="10"'#13#10'     in' +
      'kscape:pageopacity="0"'#13#10'     inkscape:pageshadow="2"'#13#10'     inksc' +
      'ape:window-width="1920"'#13#10'     inkscape:window-height="1017"'#13#10'   ' +
      '  id="namedview60"'#13#10'     showgrid="false"'#13#10'     inkscape:zoom="1' +
      '.675"'#13#10'     inkscape:cx="-84.788903"'#13#10'     inkscape:cy="102.6723' +
      '6"'#13#10'     inkscape:window-x="-8"'#13#10'     inkscape:window-y="-8"'#13#10'  ' +
      '   inkscape:window-maximized="1"'#13#10'     inkscape:current-layer="s' +
      'vg58"'#13#10'     inkscape:document-rotation="0" />'#13#10'  <defs'#13#10'     id=' +
      '"defs3">'#13#10'    <path'#13#10'       d="M5.482 31.319A18.27 18.27 0 0 1 .' +
      '109 18.358C.112 8.234 8.319.027 18.443.024s18.339 8.208 18.339 1' +
      '8.334h-10.74a7.6 7.6 0 0 0-7.599-7.593c-4.194 0-7.593 3.399-7.59' +
      '3 7.593a7.57 7.57 0 0 0 2.22 5.363h.005c1.375 1.38 2.52 1.779 5.' +
      '368 2.231h0c5.066.527 9.648 2.054 12.966 5.372h0c3.319 3.319 5.3' +
      '73 7.901 5.373 12.962 0 10.126-8.213 18.339-18.339 18.339S.109 5' +
      '4.412.109 44.286H10.85a7.6 7.6 0 0 0 7.593 7.598c4.195 0 7.599-3' +
      '.404 7.599-7.598 0-2.095-.851-3.988-2.221-5.363h-.005c-1.375-1.3' +
      '75-3.348-1.849-5.373-2.226v-.005c-4.91-.753-9.643-2.054-12.961-5' +
      '.373h0zM73.452.024l-12.97 62.601h-10.74L36.782.024h10.74l7.6 36.' +
      '663L62.712.024zm18.34 25.928h18.334l.005 18.334c0 10.127-8.213 1' +
      '8.34-18.339 18.34s-18.334-8.213-18.334-18.34h0l-.005-25.927C73.4' +
      '56 8.233 81.666.026 91.792.025s18.331 8.21 18.334 18.334H99.385a' +
      '7.6 7.6 0 0 0-7.593-7.594 7.6 7.6 0 0 0-7.594 7.594v25.927c0 4.1' +
      '95 3.399 7.594 7.594 7.594a7.6 7.6 0 0 0 7.593-7.589v-7.593h-7.5' +
      '93V25.952z"'#13#10'       transform="matrix(2 0 0 2 40 158)"'#13#10'       i' +
      'd="A"'#13#10'       fill="#fff" />'#13#10'  </defs>'#13#10'  <path'#13#10'     d="m 87.7' +
      '7,23.809 v 71.504 c 0,2.598 -2.09,4.688 -4.687,4.688 H 17.457 c ' +
      '-2.598,0 -4.687,-2.09 -4.687,-4.687 V 4.688 C 12.77,2.09 14.86,0' +
      ' 17.457,0 h 46.485 z"'#13#10'     fill="#b3b3b3"'#13#10'     id="path5"'#13#10'   ' +
      '  style="opacity:0.995" />'#13#10'  <path'#13#10'     d="M63.942 23.809L87.7' +
      '7 47.637V23.809z"'#13#10'     opacity=".4"'#13#10'     id="path7" />'#13#10'  <pat' +
      'h'#13#10'     d="M86.403 20.508L67.262 1.367C66.383.488 65.192 0 63.94' +
      '2 0v23.809H87.77c0-1.23-.488-2.422-1.367-3.301z"'#13#10'     fill="#e6' +
      'e6e6"'#13#10'     id="path9" />'#13#10'  <path'#13#10'     d="M30.115 8.758l-6.859' +
      ' 76.489-10.158 11.586 7.18-12.687 7.027-79.097c.756.362 2.979 1.' +
      '672 2.81 3.71z"'#13#10'     opacity=".4"'#13#10'     id="path11" />'#13#10'  <g'#13#10' ' +
      '    transform="matrix(0.060535,-0.050795,0.050795,0.060535,37.66' +
      '1356,29.863397)"'#13#10'     id="g35"'#13#10'     style="display:inline">'#13#10' ' +
      '   <path'#13#10'       d="m -718.984,276.544 610.062,-610.062 110.369,' +
      '108.822 -609.236,609.253 c -111.192,42.832 -101.445,5.504 -68.65' +
      '8,-83.839 -25.462,9.266 -56.817,13.497 -42.536,-24.174 z"'#13#10'     ' +
      '  fill="#fde061"'#13#10'       id="path13" />'#13#10'    <path'#13#10'       d="M ' +
      '-763.684,397.59 -719,276.602 c -1.657,13.479 -2.446,20.206 -0.33' +
      '1,21.593 5.138,3.37 33.879,-2.455 42.896,2.512 4.273,12.189 -11.' +
      '973,42.504 -5.24,49.225 6.311,6.298 52.395,-5.271 58.6,-1.353 0.' +
      '745,36.525 -15.633,38.854 8.027,38.672 l -117.154,43.776 c -10.0' +
      '8,-9.097 -25.571,-15.664 -15.174,-34.334 z"'#13#10'       fill="#fcb18' +
      '0"'#13#10'       id="path15" />'#13#10'    <path'#13#10'       d="m -623.079,348.5' +
      '75 c -55.782,18.405 -78.519,8.198 -53.357,-47.868 l 601.109,-601' +
      '.109 50.974,50.251 z"'#13#10'       fill="#fdce24"'#13#10'       id="path17"' +
      ' />'#13#10'    <path'#13#10'       d="m -623.076,348.578 598.726,-598.726 25' +
      '.814,25.469 -609.245,609.245 -22.619,8.405 z"'#13#10'       fill="#f4b' +
      '729"'#13#10'       id="path19" />'#13#10'    <path'#13#10'       d="m -727.27,416.' +
      '026 104.195,-67.448 c 1.828,6.341 -5.376,33.067 -4.644,36.381 0.' +
      '357,1.618 5.052,2.035 12.671,2.292 l -119.574,44.927 z"'#13#10'       ' +
      'fill="#df8040"'#13#10'       id="path21" />'#13#10'    <path'#13#10'       d="m -6' +
      '76.436,300.706 c 3.464,6.731 -6.175,25.521 -7.071,42.295 -0.434,' +
      '8.124 3.69,8.584 11.805,8.483 24.037,-0.876 42.898,-6.382 48.626' +
      ',-2.906 l -104.195,67.448 c -23.115,-2.147 -21.762,5.937 -20.107' +
      ',-19.333 z"'#13#10'       fill="#f3975a"'#13#10'       id="path23" />'#13#10'    <' +
      'path'#13#10'       d="m -782.898,448.649 19.411,-51.731 c 0.396,-1.055' +
      ' 4.025,-1.309 8.734,-1.309 8.875,0 9.314,0.576 7.711,10.134 -0.6' +
      '06,3.615 -0.877,7.179 -0.602,7.92 0.352,0.947 3.511,1.347 10.625' +
      ',1.347 9.076,0 10.124,0.203 10.124,1.964 0,2.768 -2.786,13.421 -' +
      '4.728,14.138 l -50.1,18.525 c -1.24,0.459 -1.488,-0.16 -1.177,-0' +
      '.988 z"'#13#10'       fill="#0f0d0b"'#13#10'       id="path25" />'#13#10'    <path' +
      #13#10'       d="m -49.377,-393.062 47.509,-47.509 c 18.819,-22.203 1' +
      '31.38,87.037 109.602,109.605 l -47.508,47.508 A 1113.83,1113.83 ' +
      '0 0 1 -49.377,-393.062 Z"'#13#10'       fill="#ec9190"'#13#10'       id="pat' +
      'h27" />'#13#10'    <path'#13#10'       d="m -17.664,-358.065 63.904,-63.904 ' +
      'c 12.902,9.508 38.58,34.333 50.759,52.774 l -62.53,62.562 c -18.' +
      '736,-17.49 -33.461,-31.859 -52.133,-51.432 z"'#13#10'       fill="#e86' +
      'c6a"'#13#10'       id="path29" />'#13#10'    <path'#13#10'       d="m 34.48,-306.6' +
      '35 62.538,-62.538 c 3.42,5.268 18.844,26.616 11.53,37.393 l -48.' +
      '323,48.316 c -8.826,-7.679 -17.496,-15.67 -25.745,-23.171 z"'#13#10'  ' +
      '     fill="#d6514f"'#13#10'       id="path31" />'#13#10'    <path'#13#10'       d=' +
      '"m -108.926,-333.514 59.549,-59.549 c 33.136,36.997 70.994,73.55' +
      '9 109.604,109.604 L 0.678,-223.91 c -38.49,-34.283 -77.593,-72.3' +
      '89 -109.604,-109.604 z"'#13#10'       fill="#dbdbdb"'#13#10'       id="path3' +
      '3" />'#13#10'  </g>'#13#10'  <path'#13#10'     id="rect848-7"'#13#10'     style="display' +
      ':inline;opacity:0.998;fill:#000000;fill-opacity:1;stroke-width:0' +
      '.877845"'#13#10'     d="m 27.942844,50.666547 c -2.303773,0 -4.158449,' +
      '1.322392 -4.158449,2.964998 V 80.04582 c 0,1.642609 1.854676,2.9' +
      '65001 4.158449,2.965001 h 53.383617 c 2.303777,0 4.158443,-1.322' +
      '392 4.158443,-2.965001 V 53.631545 c 0,-1.642606 -1.854666,-2.96' +
      '4998 -4.158443,-2.964998 z m 1.413223,4.140575 h 6.521935 l 6.52' +
      '3962,8.847226 6.523967,-8.847226 h 6.523967 V 78.870246 H 48.925' +
      '931 V 65.070252 l -6.523967,8.845777 -6.523962,-8.845777 v 13.79' +
      '9994 h -6.521935 z m 37.509243,0 h 6.523966 v 12.385539 h 6.5239' +
      '65 l -9.784935,11.677585 -9.78493,-11.677585 h 6.521934 z" />'#13#10' ' +
      ' <path'#13#10'     d="M 31.989488,84.201831 V 51.738706 h 6.275035 l 6' +
      '.275038,11.934973 6.275033,-11.934973 h 6.275037 V 84.201831 H 5' +
      '0.814594 V 65.583274 L 44.539561,77.518246 38.264523,65.583274 v' +
      ' 18.618557 z m 39.218975,0 -9.412559,-15.754162 h 6.275037 V 51.' +
      '738706 h 6.275038 v 16.708963 h 6.275037 z"'#13#10'     id="path851"'#13#10 +
      '     style="display:none;opacity:0.996;fill:#000000;stroke-width' +
      ':0.387021" />'#13#10'  <path'#13#10'     style="display:none;opacity:0.995;f' +
      'ill:#000000;fill-opacity:1;stroke-width:0.340447"'#13#10'     d="m 29.' +
      '105718,82.325946 v -39.51766 h 9.429855 L 47.96543,57.336839 57.' +
      '395282,42.808286 H 66.82514 V 82.326 H 71.4335 L 62.234711,94.33' +
      '7 53.0345,82.326 h 4.360782 V 59.661406 L 47.96543,74.189959 38.' +
      '535573,59.661406 V 82.326 Z"'#13#10'     id="path943"'#13#10'     sodipodi:n' +
      'odetypes="cccccccccccccccc" />'#13#10'</svg>'#13#10
  end
  object SVGIconImage2: TSVGIconImage
    Left = 9
    Top = 3
    Width = 175
    Height = 53
    AutoSize = False
    Proportional = True
    SVGText = 
      '<svg xmlns="http://www.w3.org/2000/svg" width="755.906" height="' +
      '226.772" viewBox="0 0 200 60">'#13#10' <defs>'#13#10'  <filter id="A">'#13#10'   <' +
      'feGaussianBlur in="SourceAlpha" stdDeviation="2"/>'#13#10'   <feOffset' +
      ' dx="2" dy="2" result="A"/>'#13#10'   <feFlood flood-color="#fff" floo' +
      'd-opacity=".6"/>'#13#10'   <feComposite in2="A" operator="in" result="' +
      'A"/>'#13#10'   <feMerge>'#13#10'    <feMergeNode in="A"/>'#13#10'    <feMergeNode ' +
      'in="SourceGraphic"/>'#13#10'   </feMerge>'#13#10'  </filter>'#13#10' </defs>'#13#10' <pa' +
      'th fill="#005e98" filter="url(#A)" transform="matrix(.8 0 0 .8 1' +
      '8 6)" d="M0 60l4.557-11.2H37.6V60zm52.8-48.8h-9.2V0h29.6v11.2H64' +
      'V60H52.8zM79.2 0h11.2v60H79.2zm30.4 0h11.2v60h-11.2zm17.2 48.8h2' +
      '9.6V60h-29.6zm52.075-13.2H162.4V24.4h11.92l-5.368-13.2H162.4V0h1' +
      '3.2L200 60h-11.2zM94.4 24.4h11.2v11.2H94.4zm32.4 0h29.598v11.2H1' +
      '26.8zm-112.325 0H37.6v11.2H9.924zm5.368-13.2L24.4 0h13.2v11.2zM1' +
      '26.8 0h29.6v11.2h-29.6z"/>'#13#10'</svg>'
  end
end
