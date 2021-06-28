object SVGSettingsForm: TSVGSettingsForm
  Left = 259
  Top = 148
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 511
  ClientWidth = 815
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 600
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object TLabel
    Left = 4
    Top = 32
    Width = 3
    Height = 15
  end
  object pc: TPageControl
    Left = 153
    Top = 41
    Width = 662
    Height = 451
    ActivePage = stGeneral
    Align = alClient
    Images = SettingsImageList
    TabOrder = 0
    object stGeneral: TTabSheet
      Caption = 'Anteprima'
      ImageIndex = 4
      ImageName = 'eye-settings'
      object RenderingGroupBox: TGroupBox
        Left = 2
        Top = 0
        Width = 238
        Height = 105
        Caption = 'External files loading options'
        TabOrder = 0
        object SearchInFolderCheckBox: TCheckBox
          Left = 15
          Top = 29
          Width = 156
          Height = 17
          Caption = 'Search in same folder'
          TabOrder = 0
        end
        object DownloadFromWebCheckBox: TCheckBox
          Left = 15
          Top = 55
          Width = 156
          Height = 17
          Caption = 'Download from WEB'
          TabOrder = 1
        end
      end
    end
    object stTheme: TTabSheet
      Caption = 'Theme'
      ImageIndex = 2
      ImageName = 'theme-light-dark'
      object ThemeLeftPanel: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 402
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object ThemesRadioGroup: TRadioGroup
          Left = 0
          Top = 0
          Width = 185
          Height = 118
          Align = alTop
          Caption = 'Theme'
          ItemIndex = 0
          Items.Strings = (
            'Same as Windows'
            'Force Dark'
            'Force Light')
          TabOrder = 0
          OnClick = ThemesRadioGroupClick
        end
        object SelectThemeRadioGroup: TRadioGroup
          Left = 0
          Top = 118
          Width = 185
          Height = 284
          Align = alClient
          Caption = 'Selected Theme'
          TabOrder = 1
          OnClick = SelectThemeRadioGroupClick
        end
      end
      object ThemeClientPanel: TPanel
        Left = 185
        Top = 0
        Width = 469
        Height = 402
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        StyleElements = []
      end
    end
    object tsFont: TTabSheet
      Caption = 'Font'
      ImageIndex = 1
      ImageName = 'alphabetical-variant'
      object XMLGroupBox: TGroupBox
        Left = 16
        Top = 164
        Width = 281
        Height = 149
        Caption = 'Testo XML'
        TabOrder = 1
        object FontLabel: TLabel
          Left = 8
          Top = 44
          Width = 57
          Height = 15
          Caption = 'Font name'
        end
        object SizeLabel: TLabel
          Left = 8
          Top = 90
          Width = 20
          Height = 15
          Caption = 'Size'
        end
        object XMLFontComboBox: TComboBox
          Left = 8
          Top = 61
          Width = 225
          Height = 22
          Style = csOwnerDrawFixed
          Sorted = True
          TabOrder = 1
          OnDrawItem = FontDrawItem
        end
        object XMLFontSizeEdit: TEdit
          Left = 8
          Top = 107
          Width = 34
          Height = 23
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 2
          Text = '12'
        end
        object XMLUpDown: TUpDown
          Left = 42
          Top = 107
          Width = 16
          Height = 23
          Associate = XMLFontSizeEdit
          Min = 8
          Max = 30
          Position = 12
          TabOrder = 3
        end
        object ShowXMLCheckBox: TCheckBox
          Left = 8
          Top = 24
          Width = 121
          Height = 17
          Caption = 'Mostra testo XML'
          TabOrder = 0
        end
      end
      object HTMLGroupBox: TGroupBox
        Left = 16
        Top = 16
        Width = 281
        Height = 121
        Caption = 'Testo HTML (default)'
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 19
          Width = 57
          Height = 15
          Caption = 'Font name'
        end
        object Label2: TLabel
          Left = 8
          Top = 65
          Width = 63
          Height = 15
          Caption = 'Dimensione'
        end
        object HTMLFontComboBox: TComboBox
          Left = 8
          Top = 36
          Width = 225
          Height = 22
          Style = csOwnerDrawFixed
          Sorted = True
          TabOrder = 0
          OnDrawItem = FontDrawItem
        end
        object HTMLFontSizeEdit: TEdit
          Left = 8
          Top = 82
          Width = 34
          Height = 23
          Alignment = taRightJustify
          NumbersOnly = True
          TabOrder = 1
          Text = '12'
        end
        object HTMLUpDown: TUpDown
          Left = 42
          Top = 82
          Width = 16
          Height = 23
          Associate = HTMLFontSizeEdit
          Min = 8
          Max = 30
          Position = 12
          TabOrder = 2
        end
      end
    end
    object tsColors: TTabSheet
      Caption = 'Text color'
      ImageName = 'palette'
      object VertSplitter: TSplitter
        Left = 193
        Top = 0
        Width = 4
        Height = 402
        MinSize = 100
        ExplicitLeft = 143
        ExplicitHeight = 366
      end
      object paLeft: TPanel
        Left = 0
        Top = 0
        Width = 193
        Height = 402
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object paElements: TPanel
          Left = 0
          Top = 0
          Width = 193
          Height = 244
          Align = alClient
          BevelOuter = bvLowered
          TabOrder = 0
          object BoxElements: TListBox
            Left = 1
            Top = 17
            Width = 191
            Height = 226
            Align = alClient
            BevelOuter = bvNone
            ItemHeight = 15
            TabOrder = 0
            OnClick = BoxElementsClick
          end
          object paElemTitle: TPanel
            Left = 1
            Top = 1
            Width = 191
            Height = 16
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Elements'
            TabOrder = 1
          end
        end
        object ElementColorGroupBox: TGroupBox
          Left = 0
          Top = 244
          Width = 193
          Height = 116
          Align = alBottom
          Caption = 'Element colors'
          TabOrder = 1
          DesignSize = (
            193
            116)
          object ForegroundColorLabel: TLabel
            Left = 9
            Top = 20
            Width = 94
            Height = 15
            Caption = 'Foreground Color'
          end
          object BackgroundColorLabel: TLabel
            Left = 9
            Top = 65
            Width = 96
            Height = 15
            Caption = 'Background Color'
          end
          object ForegroundColorBox: TColorBox
            Left = 9
            Top = 36
            Width = 174
            Height = 22
            NoneColorColor = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 20
            TabOrder = 0
            OnSelect = ColorBoxSelect
          end
          object BackgroundColorBox: TColorBox
            Left = 9
            Top = 81
            Width = 174
            Height = 22
            NoneColorColor = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 20
            TabOrder = 1
            OnSelect = ColorBoxSelect
          end
        end
        object ResetPanel: TPanel
          Left = 0
          Top = 360
          Width = 193
          Height = 42
          Align = alBottom
          TabOrder = 2
          DesignSize = (
            193
            42)
          object ResetButton: TButton
            Left = 9
            Top = 6
            Width = 174
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Reset to default colors'
            TabOrder = 0
            OnClick = ResetButtonClick
          end
        end
      end
      object paAttributesContainer: TPanel
        Left = 197
        Top = 0
        Width = 457
        Height = 402
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object paAttributes: TPanel
          Left = 0
          Top = 0
          Width = 457
          Height = 65
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 0
          object TLabel
            Left = 8
            Top = 4
            Width = 3
            Height = 15
          end
          object cbTextAttrib: TGroupBox
            Left = 5
            Top = 4
            Width = 180
            Height = 58
            Caption = 'Text Attributes'
            TabOrder = 0
            object cbBold: TCheckBox
              Left = 5
              Top = 16
              Width = 78
              Height = 17
              Caption = '&Bold'
              TabOrder = 0
              OnClick = cbFontStyleClick
            end
            object cbItalic: TCheckBox
              Left = 5
              Top = 36
              Width = 78
              Height = 17
              Caption = '&Italic'
              TabOrder = 1
              OnClick = cbFontStyleClick
            end
            object cbUnderline: TCheckBox
              Left = 88
              Top = 16
              Width = 79
              Height = 17
              Caption = '&Underline'
              TabOrder = 2
              OnClick = cbFontStyleClick
            end
            object cbStrikeOut: TCheckBox
              Left = 88
              Top = 36
              Width = 79
              Height = 17
              Caption = 'Stri&keOut'
              TabOrder = 3
              OnClick = cbFontStyleClick
            end
          end
          object gbWhiteSpace: TGroupBox
            Left = 191
            Top = 4
            Width = 169
            Height = 58
            Caption = 'Use WhiteSpace color for'
            TabOrder = 1
            object cbForeground: TCheckBox
              Left = 8
              Top = 16
              Width = 128
              Height = 17
              Caption = '&Foreground color'
              TabOrder = 0
              OnClick = cbForegroundClick
            end
            object cbBackground: TCheckBox
              Left = 8
              Top = 36
              Width = 128
              Height = 17
              Caption = '&Background color'
              TabOrder = 1
              OnClick = cbBackgroundClick
            end
          end
        end
        object SynEdit: TSynEdit
          Left = 0
          Top = 65
          Width = 457
          Height = 337
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 1
          OnClick = SynEditClick
          OnKeyUp = SynEditKeyUp
          CodeFolding.GutterShapeSize = 11
          CodeFolding.CollapsedLineColor = clGrayText
          CodeFolding.FolderBarLinesColor = clGrayText
          CodeFolding.IndentGuidesColor = clGray
          CodeFolding.IndentGuides = True
          CodeFolding.ShowCollapsedLine = False
          CodeFolding.ShowHintMark = True
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Terminal'
          Gutter.Font.Style = []
          FontSmoothing = fsmNone
        end
      end
    end
    object tsPDFLayout: TTabSheet
      Caption = 'PDF layout'
      ImageIndex = 6
      ImageName = 'pdf-box'
      object MarginTopLabel: TLabel
        Left = 362
        Top = 3
        Width = 90
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Margine alto'
      end
      object MarginLeftLabel: TLabel
        Left = 218
        Top = 94
        Width = 90
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Margine sinistro'
      end
      object MarginRightLabel: TLabel
        Left = 505
        Top = 94
        Width = 90
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Margine destro'
      end
      object MarginBottomLabel: TLabel
        Left = 362
        Top = 238
        Width = 90
        Height = 15
        Alignment = taCenter
        AutoSize = False
        Caption = 'Margine basso'
      end
      object OrientationRadioGroup: TRadioGroup
        Left = 16
        Top = 16
        Width = 193
        Height = 81
        Caption = 'Orientamento'
        Items.Strings = (
          'Verticale (portrait)'
          'Orizzontale (landscape)')
        TabOrder = 0
        OnClick = OrientationRadioGroupClick
      end
      object SVGIconPosition: TSVGIconImage
        Left = 313
        Top = 51
        Width = 186
        Height = 154
        AutoSize = False
        Proportional = True
        ImageList = OrientationImageList
        ImageIndex = 0
      end
      object MarginLeftEdit: TNumberBox
        Left = 218
        Top = 115
        Width = 90
        Height = 23
        Alignment = taRightJustify
        Mode = nbmFloat
        TabOrder = 3
        SpinButtonOptions.Placement = nbspInline
      end
      object MarginRightEdit: TNumberBox
        Left = 505
        Top = 115
        Width = 90
        Height = 23
        Alignment = taRightJustify
        Mode = nbmFloat
        TabOrder = 4
        SpinButtonOptions.Placement = nbspInline
      end
      object MarginTopEdit: TNumberBox
        Left = 362
        Top = 22
        Width = 90
        Height = 23
        Alignment = taRightJustify
        Mode = nbmFloat
        TabOrder = 2
        SpinButtonOptions.Placement = nbspInline
      end
      object MarginBottomEdit: TNumberBox
        Left = 362
        Top = 211
        Width = 90
        Height = 23
        Alignment = taRightJustify
        Mode = nbmFloat
        TabOrder = 5
        SpinButtonOptions.Placement = nbspInline
      end
      object PaperSizeRadioGroup: TRadioGroup
        Left = 16
        Top = 103
        Width = 193
        Height = 266
        Caption = 'Dimensione pagina'
        Items.Strings = (
          'A4'
          'A5'
          'A3'
          'A2'
          'A1'
          'A0'
          'Letter'
          'Legal')
        TabOrder = 1
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 492
    Width = 815
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object MenuButtonGroup: TButtonGroup
    Left = 0
    Top = 41
    Width = 153
    Height = 451
    Align = alLeft
    BevelOuter = bvNone
    BorderStyle = bsNone
    ButtonHeight = 48
    ButtonOptions = [gboFullSize, gboGroupStyle, gboShowCaptions]
    Images = SettingsImageList
    Items = <
      item
        Caption = '  Back'
        ImageIndex = 3
        ImageName = 'arrow-left'
      end
      item
        Caption = '  Preview'
        ImageIndex = 4
        ImageName = 'eye-settings'
      end
      item
        Caption = '  Theme'
        ImageIndex = 2
        ImageName = 'theme-light-dark'
      end
      item
        Caption = '  Font'
        ImageIndex = 1
        ImageName = 'alphabetical-variant'
      end
      item
        Caption = '  Text colors'
        ImageIndex = 0
        ImageName = 'palette'
      end
      item
        Caption = 'PDF Layout'
        ImageIndex = 6
        ImageName = 'pdf-box'
      end>
    TabOrder = 2
    OnButtonClicked = MenuButtonGroupButtonClicked
  end
  object TitlePanel: TPanel
    Left = 0
    Top = 0
    Width = 815
    Height = 41
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Settings'
    TabOrder = 3
  end
  object OpenDialog: TOpenDialog
    Left = 592
    Top = 352
  end
  object SettingsImageList: TSVGIconImageList
    Size = 36
    SVGIconItems = <
      item
        IconName = 'palette'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-pa' +
          'lette" width="24" height="24" viewBox="0 0 24 24">'#13#10'<path d="M17' +
          '.5,12A1.5,1.5 0 0,1 16,10.5A1.5,1.5 0 0,1 17.5,9A1.5,1.5 0 0,1 1' +
          '9,10.5A1.5,1.5 0 0,1 17.5,12M14.5,8A1.5,1.5 0 0,1 13,6.5A1.5,1.5' +
          ' 0 0,1 14.5,5A1.5,1.5 0 0,1 16,6.5A1.5,1.5 0 0,1 14.5,8M9.5,8A1.' +
          '5,1.5 0 0,1 8,6.5A1.5,1.5 0 0,1 9.5,5A1.5,1.5 0 0,1 11,6.5A1.5,1' +
          '.5 0 0,1 9.5,8M6.5,12A1.5,1.5 0 0,1 5,10.5A1.5,1.5 0 0,1 6.5,9A1' +
          '.5,1.5 0 0,1 8,10.5A1.5,1.5 0 0,1 6.5,12M12,3A9,9 0 0,0 3,12A9,9' +
          ' 0 0,0 12,21A1.5,1.5 0 0,0 13.5,19.5C13.5,19.11 13.35,18.76 13.1' +
          '1,18.5C12.88,18.23 12.73,17.88 12.73,17.5A1.5,1.5 0 0,1 14.23,16' +
          'H16A5,5 0 0,0 21,11C21,6.58 16.97,3 12,3Z" />'#13#10'</svg>'
      end
      item
        IconName = 'alphabetical-variant'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-al' +
          'phabetical-variant" width="24" height="24" viewBox="0 0 24 24">'#13 +
          #10'<path d="M3 7A2 2 0 0 0 1 9V17H3V13H5V17H7V9A2 2 0 0 0 5 7H3M3 ' +
          '9H5V11H3M15 10.5V9A2 2 0 0 0 13 7H9V17H13A2 2 0 0 0 15 15V13.5A1' +
          '.54 1.54 0 0 0 13.5 12A1.54 1.54 0 0 0 15 10.5M13 15H11V13H13V15' +
          'M13 11H11V9H13M19 7A2 2 0 0 0 17 9V15A2 2 0 0 0 19 17H21A2 2 0 0' +
          ' 0 23 15V14H21V15H19V9H21V10H23V9A2 2 0 0 0 21 7Z" />'#13#10'</svg>'
      end
      item
        IconName = 'theme-light-dark'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-th' +
          'eme-light-dark" width="24" height="24" viewBox="0 0 24 24">'#13#10'<pa' +
          'th d="M7.5,2C5.71,3.15 4.5,5.18 4.5,7.5C4.5,9.82 5.71,11.85 7.53' +
          ',13C4.46,13 2,10.54 2,7.5A5.5,5.5 0 0,1 7.5,2M19.07,3.5L20.5,4.9' +
          '3L4.93,20.5L3.5,19.07L19.07,3.5M12.89,5.93L11.41,5L9.97,6L10.39,' +
          '4.3L9,3.24L10.75,3.12L11.33,1.47L12,3.1L13.73,3.13L12.38,4.26L12' +
          '.89,5.93M9.59,9.54L8.43,8.81L7.31,9.59L7.65,8.27L6.56,7.44L7.92,' +
          '7.35L8.37,6.06L8.88,7.33L10.24,7.36L9.19,8.23L9.59,9.54M19,13.5A' +
          '5.5,5.5 0 0,1 13.5,19C12.28,19 11.15,18.6 10.24,17.93L17.93,10.2' +
          '4C18.6,11.15 19,12.28 19,13.5M14.6,20.08L17.37,18.93L17.13,22.28' +
          'L14.6,20.08M18.93,17.38L20.08,14.61L22.28,17.15L18.93,17.38M20.0' +
          '8,12.42L18.94,9.64L22.28,9.88L20.08,12.42M9.63,18.93L12.4,20.08L' +
          '9.87,22.27L9.63,18.93Z" />'#13#10'</svg>'
      end
      item
        IconName = 'arrow-left'
        SVGText = 
          '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" id="mdi-ar' +
          'row-left" width="24" height="24" viewBox="0 0 24 24">'#13#10'<path d="' +
          'M20,11V13H8L13.5,18.5L12.08,19.92L4.16,12L12.08,4.08L13.5,5.5L8,' +
          '11H20Z" />'#13#10'</svg>'
      end
      item
        IconName = 'eye-settings'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-eye-settings" w' +
          'idth="24" height="24" viewBox="0 0 24 24"><path d="M12,9A3,3 0 0' +
          ',0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9M12,17A5,5' +
          ' 0 0,1 7,12A5,5 0 0,1 12,7A5,5 0 0,1 17,12A5,5 0 0,1 12,17M12,4.' +
          '5C7.14,4.5 2.78,7.5 1,12C3.39,18.08 10.25,21.06 16.33,18.67C19.3' +
          '8,17.47 21.8,15.06 23,12C21.22,7.5 16.86,4.5 12,4.5M7,22H9V24H7V' +
          '22M11,22H13V24H11V22M15,22H17V24H15V22Z" /></svg>'
      end
      item
        IconName = 'developer-board'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-developer-board' +
          '" width="24" height="24" viewBox="0 0 24 24"><path d="M22,9V7H20' +
          'V5A2,2 0 0,0 18,3H4A2,2 0 0,0 2,5V19A2,2 0 0,0 4,21H18A2,2 0 0,0' +
          ' 20,19V17H22V15H20V13H22V11H20V9H22M18,19H4V5H18V19M6,13H11V17H6' +
          'V13M12,7H16V10H12V7M6,7H11V12H6V7M12,11H16V17H12V11Z" /></svg>'
      end
      item
        IconName = 'pdf-box'
        SVGText = 
          '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W' +
          '3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg' +
          '11.dtd"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="htt' +
          'p://www.w3.org/1999/xlink" version="1.1" id="mdi-pdf-box" width=' +
          '"24" height="24" viewBox="0 0 24 24"><path d="M12,10.5H13V13.5H1' +
          '2V10.5M7,11.5H8V10.5H7V11.5M20,6V18A2,2 0 0,1 18,20H6A2,2 0 0,1 ' +
          '4,18V6A2,2 0 0,1 6,4H18A2,2 0 0,1 20,6M9.5,10.5A1.5,1.5 0 0,0 8,' +
          '9H5.5V15H7V13H8A1.5,1.5 0 0,0 9.5,11.5V10.5M14.5,10.5A1.5,1.5 0 ' +
          '0,0 13,9H10.5V15H13A1.5,1.5 0 0,0 14.5,13.5V10.5M18.5,9H15.5V15H' +
          '17V13H18.5V11.5H17V10.5H18.5V9Z" /></svg>'
      end>
    Scaled = True
    Left = 492
    Top = 368
  end
  object OrientationImageList: TSVGIconImageList
    Size = 128
    SVGIconItems = <
      item
        IconName = 'portrait'
        SVGText = 
          #65279'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www' +
          '.w3.org/1999/xlink" version="1.1" x="0px" y="0px" viewBox="0 0 1' +
          '00 125" enable-background="new 0 0 100 100" xml:space="preserve"' +
          '>'#13#10'  <path d="M85,92.5V20c0-0.7-0.3-1.3-0.7-1.8L71.8,5.7C71.3,5.' +
          '3,70.7,5,70,5H17.5C16.1,5,15,6.1,15,7.5v85c0,1.4,1.1,2.5,2.5,2.5' +
          'h65  C83.9,95,85,93.9,85,92.5z M70,11l9,9h-9V11z M80,90H20V10h45' +
          'v12.5c0,1.4,1.1,2.5,2.5,2.5H80V90z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'landscape'
        SVGText = 
          #65279'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www' +
          '.w3.org/1999/xlink" version="1.1" x="0px" y="0px" viewBox="0 0 1' +
          '00 125" enable-background="new 0 0 100 100" xml:space="preserve"' +
          '>'#13#10'  <path d="M81.8,15.7C81.3,15.3,80.7,15,80,15H7.5C6.1,15,5,16' +
          '.1,5,17.5v65C5,83.9,6.1,85,7.5,85h85c1.4,0,2.5-1.1,2.5-2.5V30  c' +
          '0-0.7-0.3-1.3-0.7-1.8L81.8,15.7z M80,21l9,9h-9V21z M90,80H10V20h' +
          '65v12.5c0,1.4,1.1,2.5,2.5,2.5H90V80z"/>'#13#10'</svg>'#13#10
      end>
    Scaled = True
    Left = 356
    Top = 336
  end
end
