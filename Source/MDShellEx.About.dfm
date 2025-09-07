object FrmAbout: TFrmAbout
  Left = 651
  Top = 323
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 312
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDefault
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    436
    312)
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
    Left = 396
    Top = 64
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version'
  end
  object SVGIconImage1: TSVGIconImage
    Left = 383
    Top = 8
    Width = 52
    Height = 49
    AutoSize = False
    SVGText = 
      '<?xml version="1.0" standalone="no"?>'#13#10'<svg width="100" height="' +
      '100" version="1.1">'#13#10'  <path d="m 87.77,23.809 v 71.504 c 0,2.59' +
      '8 -2.09,4.688 -4.687,4.688 H 17.457 c -2.598,0 -4.687,-2.09 -4.6' +
      '87,-4.687 V 4.688 C 12.77,2.09 14.86,0 17.457,0 h 46.485 z" fill' +
      '="#b3b3b3" id="path5" style="opacity:0.995"/>'#13#10'  <path d="M63.94' +
      '2 23.809L87.77 47.637V23.809z" opacity=".4" id="path7"/>'#13#10'  <pat' +
      'h d="M86.403 20.508L67.262 1.367C66.383.488 65.192 0 63.942 0v23' +
      '.809H87.77c0-1.23-.488-2.422-1.367-3.301z" fill="#e6e6e6" id="pa' +
      'th9"/>'#13#10'  <path d="M30.115 8.758l-6.859 76.489-10.158 11.586 7.1' +
      '8-12.687 7.027-79.097c.756.362 2.979 1.672 2.81 3.71z" opacity="' +
      '.4" id="path11"/>'#13#10'  <g transform="matrix(0.060535,-0.050795,0.0' +
      '50795,0.060535,37.661356,29.863397)" id="g35" style="display:inl' +
      'ine">'#13#10'    <path d="m -718.984,276.544 610.062,-610.062 110.369,' +
      '108.822 -609.236,609.253 c -111.192,42.832 -101.445,5.504 -68.65' +
      '8,-83.839 -25.462,9.266 -56.817,13.497 -42.536,-24.174 z" fill="' +
      '#fde061" id="path13"/>'#13#10'    <path d="M -763.684,397.59 -719,276.' +
      '602 c -1.657,13.479 -2.446,20.206 -0.331,21.593 5.138,3.37 33.87' +
      '9,-2.455 42.896,2.512 4.273,12.189 -11.973,42.504 -5.24,49.225 6' +
      '.311,6.298 52.395,-5.271 58.6,-1.353 0.745,36.525 -15.633,38.854' +
      ' 8.027,38.672 l -117.154,43.776 c -10.08,-9.097 -25.571,-15.664 ' +
      '-15.174,-34.334 z" fill="#fcb180" id="path15"/>'#13#10'    <path d="m ' +
      '-623.079,348.575 c -55.782,18.405 -78.519,8.198 -53.357,-47.868 ' +
      'l 601.109,-601.109 50.974,50.251 z" fill="#fdce24" id="path17"/>' +
      #13#10'    <path d="m -623.076,348.578 598.726,-598.726 25.814,25.469' +
      ' -609.245,609.245 -22.619,8.405 z" fill="#f4b729" id="path19"/>'#13 +
      #10'    <path d="m -727.27,416.026 104.195,-67.448 c 1.828,6.341 -5' +
      '.376,33.067 -4.644,36.381 0.357,1.618 5.052,2.035 12.671,2.292 l' +
      ' -119.574,44.927 z" fill="#df8040" id="path21"/>'#13#10'    <path d="m' +
      ' -676.436,300.706 c 3.464,6.731 -6.175,25.521 -7.071,42.295 -0.4' +
      '34,8.124 3.69,8.584 11.805,8.483 24.037,-0.876 42.898,-6.382 48.' +
      '626,-2.906 l -104.195,67.448 c -23.115,-2.147 -21.762,5.937 -20.' +
      '107,-19.333 z" fill="#f3975a" id="path23"/>'#13#10'    <path d="m -782' +
      '.898,448.649 19.411,-51.731 c 0.396,-1.055 4.025,-1.309 8.734,-1' +
      '.309 8.875,0 9.314,0.576 7.711,10.134 -0.606,3.615 -0.877,7.179 ' +
      '-0.602,7.92 0.352,0.947 3.511,1.347 10.625,1.347 9.076,0 10.124,' +
      '0.203 10.124,1.964 0,2.768 -2.786,13.421 -4.728,14.138 l -50.1,1' +
      '8.525 c -1.24,0.459 -1.488,-0.16 -1.177,-0.988 z" fill="#0f0d0b"' +
      ' id="path25"/>'#13#10'    <path d="m -49.377,-393.062 47.509,-47.509 c' +
      ' 18.819,-22.203 131.38,87.037 109.602,109.605 l -47.508,47.508 A' +
      ' 1113.83,1113.83 0 0 1 -49.377,-393.062 Z" fill="#ec9190" id="pa' +
      'th27"/>'#13#10'    <path d="m -17.664,-358.065 63.904,-63.904 c 12.902' +
      ',9.508 38.58,34.333 50.759,52.774 l -62.53,62.562 c -18.736,-17.' +
      '49 -33.461,-31.859 -52.133,-51.432 z" fill="#e86c6a" id="path29"' +
      '/>'#13#10'    <path d="m 34.48,-306.635 62.538,-62.538 c 3.42,5.268 18' +
      '.844,26.616 11.53,37.393 l -48.323,48.316 c -8.826,-7.679 -17.49' +
      '6,-15.67 -25.745,-23.171 z" fill="#d6514f" id="path31"/>'#13#10'    <p' +
      'ath d="m -108.926,-333.514 59.549,-59.549 c 33.136,36.997 70.994' +
      ',73.559 109.604,109.604 L 0.678,-223.91 c -38.49,-34.283 -77.593' +
      ',-72.389 -109.604,-109.604 z" fill="#dbdbdb" id="path33"/>'#13#10'  </' +
      'g>'#13#10'  <path id="rect848-7" style="display:inline;opacity:0.998;f' +
      'ill:#000000;fill-opacity:1;stroke-width:0.877845" d="m 27.942844' +
      ',50.666547 c -2.303773,0 -4.158449,1.322392 -4.158449,2.964998 V' +
      ' 80.04582 c 0,1.642609 1.854676,2.965001 4.158449,2.965001 h 53.' +
      '383617 c 2.303777,0 4.158443,-1.322392 4.158443,-2.965001 V 53.6' +
      '31545 c 0,-1.642606 -1.854666,-2.964998 -4.158443,-2.964998 z m ' +
      '1.413223,4.140575 h 6.521935 l 6.523962,8.847226 6.523967,-8.847' +
      '226 h 6.523967 V 78.870246 H 48.925931 V 65.070252 l -6.523967,8' +
      '.845777 -6.523962,-8.845777 v 13.799994 h -6.521935 z m 37.50924' +
      '3,0 h 6.523966 v 12.385539 h 6.523965 l -9.784935,11.677585 -9.7' +
      '8493,-11.677585 h 6.521934 z"/>'#13#10'  <path d="M 31.989488,84.20183' +
      '1 V 51.738706 h 6.275035 l 6.275038,11.934973 6.275033,-11.93497' +
      '3 h 6.275037 V 84.201831 H 50.814594 V 65.583274 L 44.539561,77.' +
      '518246 38.264523,65.583274 v 18.618557 z m 39.218975,0 -9.412559' +
      ',-15.754162 h 6.275037 V 51.738706 h 6.275038 v 16.708963 h 6.27' +
      '5037 z" id="path851" style="display:none;opacity:0.996;fill:#000' +
      '000;stroke-width:0.387021"/>'#13#10'  <path style="display:none;opacit' +
      'y:0.995;fill:#000000;fill-opacity:1;stroke-width:0.340447" d="m ' +
      '29.105718,82.325946 v -39.51766 h 9.429855 L 47.96543,57.336839 ' +
      '57.395282,42.808286 H 66.82514 V 82.326 H 71.4335 L 62.234711,94' +
      '.337 53.0345,82.326 h 4.360782 V 59.661406 L 47.96543,74.189959 ' +
      '38.535573,59.661406 V 82.326 Z" id="path943"/>'#13#10'</svg>'#13#10
  end
  object SVGIconImage2: TSVGIconImage
    Left = 9
    Top = 3
    Width = 175
    Height = 53
    AutoSize = False
    SVGText = 
      '<?xml version="1.0" encoding="UTF-8"?>'#13#10'<svg version="1.1" viewB' +
      'ox="0 0 1500 500" xmlns="http://www.w3.org/2000/svg">'#13#10' <path d=' +
      '"m 29.959524,376.13727c-0.76451-0.65379-6.0819-10.147-9.7722-17.' +
      '448-12.307-24.347-18.221-44.831-19.118-66.237-0.86096-20.522 6.3' +
      '28-41.535 20.128-58.833 16.357-20.505 41.513-34.639 70.082-39.37' +
      '6 7.1343-1.183 11.248-1.5176 20.132-1.637 4.6266-0.0622 8.4572-0' +
      '.15828 8.5124-0.21351 0.0553-0.0552-0.1747-1.3857-0.51098-2.9567' +
      '-0.53126-2.4817-0.61063-3.5816-0.60524-8.387 7e-3 -6.2771 0.3420' +
      '2-8.2278 2.2667-13.199 1.2078-3.1196 1.6207-3.5926 3.0344-3.4764' +
      'l1.0134 0.0833 3.0432 5.9258c5.5025 10.714 10.216 17.563 17.574 ' +
      '25.534 1.6238 1.7589 1.9119 1.962 2.7833 1.962 0.64759 0 1.1636-' +
      '0.19159 1.5461-0.57405l0.57407-0.57406-0.70016-2.8365c-3.6152-14' +
      '.647-2.2014-26.711 4.2573-36.329 2.2951-3.4176 3.1823-4.3009 4.3' +
      '2-4.3009 1.0179 0 1.6943 0.50473 1.8776 1.4011 0.0644 0.31581 0.' +
      '43375 2.2334 0.82046 4.2614 1.8272 9.5824 5.785 20.44 10.921 29.' +
      '961 4.0554 7.5178 7.9975 13.592 16.45 25.346 7.444 10.352 8.7704' +
      ' 12.251 10.854 15.539 7.0094 11.059 9.6667 18.641 9.6451 27.522-' +
      '0.0113 4.6803-0.5214 7.6596-2.0764 12.127-1.0191 2.9278-3.5089 8' +
      '.1077-4.1546 8.6438-0.22033 0.18285-0.85331 0.29518-1.4066 0.249' +
      '57l-1.006-0.083-5.5398-11.061c-6.3211-12.621-9.0304-17.596-10.14' +
      '8-18.633-0.59271-0.54997-1.0452-0.72432-1.8808-0.72432-0.98261 0' +
      '-1.2426 0.14785-2.432 1.3827-2.3867 2.4779-3.8608 6.6791-3.3356 ' +
      '9.5062 0.36987 1.991 0.51832 2.2984 3.5294 7.309 10.68 17.771 14' +
      '.324 32.016 11.222 43.858-1.5491 5.9143-5.8604 11.878-11.474 15.' +
      '87-2.5304 1.7998-3.3379 1.9794-4.2996 0.95558l-0.61494-0.65449 0' +
      '.50117-3.2302c0.65217-4.2032 0.66424-10.016 0.0271-13.035-2.4378' +
      '-11.55-11.258-23.668-24.147-33.174-8.0866-5.9645-17.017-10.226-2' +
      '5.81-12.315-7.9227-1.8827-19.198-2.0312-28.97-0.38191-19.669 3.3' +
      '199-36.586 14.363-48.172 31.447-7.7223 11.386-13.611 25.896-16.1' +
      '91 39.9-1.2558 6.8129-1.5873 10.664-1.5873 18.436 0 7.276 0.3376' +
      '8 11.622 1.3195 16.984 0.78891 4.3082 0.78674 4.7164-0.02831 5.3' +
      '574-0.87842 0.69101-1.7289 0.72922-2.4526 0.11033z" fill="#9c9b9' +
      'b" stroke="#7a7a7a" stroke-width="2"/>'#13#10' <path d="m 209.67,498.7' +
      '6727c-49.73-2.4409-94.499-24.132-120.6-58.433-10.493-13.787-18.0' +
      '28-29.405-21.983-45.562-3.3018-13.493-4.5368-30.212-3.0544-41.34' +
      '9 1.7015-12.783 6.6481-26.898 12.468-35.579 8.5726-12.788 21.628' +
      '-21.916 34.982-24.461 4.1317-0.78725 7.7046-1.0841 8.5095-0.7069' +
      '2 1.0166 0.47634 1.258 1.5346 0.68668 3.0122-1.5375 3.9774-2.547' +
      ' 7.1288-3.1404 9.8039-4.627 20.86 4.2672 41.937 23.959 56.779 22' +
      '.585 17.022 55.759 22.871 85.69 15.111 27.034-7.0098 47.752-24.1' +
      '84 54.636-45.288 2.0096-6.1608 2.3807-8.6319 2.3739-15.805-6e-3 ' +
      '-5.7157-0.0603-6.4431-0.71715-9.5114-0.3912-1.8272-1.3184-5.205-' +
      '2.0606-7.506-5.4327-16.845-15.016-33.609-30.696-53.696-19.119-24' +
      '.493-33.999-44.335-46.613-62.154-41.496-58.622-60.928-99.326-60.' +
      '937-127.64-7e-3 -22.885 12.471-38.088 38.327-46.7 21.684-7.2219 ' +
      '54.628-9.6274 97.438-7.1147 17.919 1.0517 47.601 3.8904 48.503 4' +
      '.6385 1.0235 0.84942 0.94317 2.8249-0.13282 3.266-0.24024 0.0984' +
      '29-2.57 0.58531-5.1774 1.0818-19.711 3.7538-32.714 9.5741-41.244' +
      ' 18.462-4.7938 4.9944-7.7248 10.472-9.1962 17.186-3.8243 17.452 ' +
      '4.0794 41.758 24.368 74.935 8.5349 13.957 18.359 28.39 37.103 54' +
      '.509 4.7815 6.6632 11.436 15.967 14.786 20.675 3.3509 4.7077 7.3' +
      '44 10.159 8.8736 12.115 8.5163 10.888 17.588 24.64 23.713 35.95 ' +
      '17.538 32.378 24.604 63.991 21.134 94.548-0.21386 1.8831-0.5746 ' +
      '5.1422-0.80164 7.2427-2.2681 20.983-9.8142 43.451-21.318 63.471-' +
      '14.952 26.022-36.383 47.977-61.367 62.865-30.6 18.235-70.426 27.' +
      '724-108.51 25.855z" fill="#3ba08b" stroke="#227b69" stroke-width' +
      '="2"/>'#13#10' <path d="m 565.03665,411.47 21.235-52.192h153.98v52.192' +
      'zm246.04-227.41h-42.872v-52.192h137.93v52.192h-42.872v227.41h-52' +
      '.192zm123.03-52.192h52.192v279.6h-52.192zm141.66 0h52.192v279.6h' +
      '-52.192zm80.152 227.41h137.93v52.192h-137.93zm242.67-61.512h-76.' +
      '773v-52.192h55.547l-25.015-61.512h-30.532v-52.192h61.512l113.7 2' +
      '79.6h-52.192zm-393.66-52.192h52.192v52.192h-52.192zm150.99 0h137' +
      '.93v52.192h-137.93zm-523.44 0h107.76v52.192h-128.97zm25.015-61.5' +
      '12 21.235-52.192h61.512v52.192zm498.42-52.192h137.93v52.192h-137' +
      '.93z" fill="#3ba08b" stroke="#227b69" stroke-width="4"/>'#13#10'</svg>' +
      #13#10
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 261
    Width = 436
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object btnOK: TStyledButton
      Left = 351
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'CLOSE'
      Default = True
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnIssues: TStyledButton
      Left = 8
      Top = 8
      Width = 125
      Height = 25
      Caption = 'Submit issue...'
      ImageIndex = 0
      TabOrder = 0
      OnClick = btnIssuesClick
    end
    object btnCheckUpdates: TStyledButton
      Left = 139
      Top = 8
      Width = 200
      Height = 25
      Caption = 'Check for updates'
      ImageIndex = 3
      TabOrder = 1
      Visible = False
      OnClick = btnCheckUpdatesClick
    end
  end
  object MemoCopyRights: TMemo
    Left = 8
    Top = 88
    Width = 427
    Height = 166
    Anchors = [akLeft, akTop, akBottom]
    Color = clBtnFace
    Lines.Strings = (
      'Author:'
      'Carlo Barazzetta'
      'https://github.com/EtheaDev/MarkdownShellExtensions'
      'Copyright '#169' 2021-2025 all rights reserved.'
      'Contributors:'
      'Ariel Montes'
      ''
      'Other libraries from Ethea:'
      'SVGIconImageList'
      'https://github.com/EtheaDev/SVGIconImageList/'
      ''
      'StyledComponents'
      'https://github.com/EtheaDev/StyledComponents'
      ''
      'Delphi MarkdownProcessor'
      'https://github.com/EtheaDev/MarkdownProcessor'
      ''
      'Third parties libraries:'
      'OpenSLL Library: Cryptography and SSL/TLS Toolkit'
      'Copyright '#169' 1998-2018 The OpenSSL Project.  All rights reserved.'
      ''
      'Delphi Markdown'
      'https://github.com/grahamegrieve/delphi-markdown'
      
        'Copyright (c) 2011+, Health Intersections Pty Ltd All rights res' +
        'erved'
      ''
      'Delphi Preview Handler'
      'https://github.com/RRUZ/delphi-preview-handler'
      'The Initial Developer of the Original Code is Rodrigo Ruz V.'
      
        'Portions created by Rodrigo Ruz V. are Copyright '#169' 2011-2023 Rod' +
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
      
        'Image32 Library - http://www.angusj.com/delphi/image32/Docs/_Bod' +
        'y.htm'
      'Copyright '#169'2019-2025 Angus Johnson.'
      ''
      'dzlib - https://sourceforge.net/projects/dzlib/'
      'Copyright (c) Thomas Mueller'
      ''
      'HTMLViewer - https://github.com/BerndGabriel/HtmlViewer'
      'Copyright (c) 1995 - 2008 by L. David Baldwin'
      'Copyright (c) 1995 - 2023 by Anders Melander (DitherUnit.pas)'
      'Copyright (c) 1995 - 2023 by Ron Collins (HtmlGif1.pas)'
      'Copyright (c) 2008 - 2009 by Sebastian Zierer (Delphi 2009 Port)'
      'Copyright (c) 2008 - 2010 by Arvid Winkelsdorf (Fixes)'
      'Copyright (c) 2009 - 2023 by HtmlViewer Team')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object LinkLabel1: TLinkLabel
    Left = 8
    Top = 62
    Width = 302
    Height = 19
    Caption = 
      '<a href="https://github.com/EtheaDev/MarkdownShellExtensions">ht' +
      'tps://github.com/EtheaDev/MarkdownShellExtensions</a>'
    TabOrder = 2
    UseVisualStyle = True
    OnLinkClick = LinkLabel1LinkClick
  end
end
