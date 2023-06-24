inherited FrmPreview: TFrmPreview
  Left = 522
  Top = 286
  ClientHeight = 617
  ClientWidth = 617
  DoubleBuffered = True
  Font.Name = 'Segoe UI'
  OnResize = FormResize
  ExplicitWidth = 633
  ExplicitHeight = 656
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 180
    Width = 617
    Height = 6
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    MinSize = 100
    OnMoved = SplitterMoved
    ExplicitTop = 329
    ExplicitWidth = 888
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 656
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 656
      Height = 35
      Align = alClient
      AutoSize = True
      ButtonHeight = 30
      ButtonWidth = 86
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = SVGIconImageList
      List = True
      ShowCaptions = True
      TabOrder = 0
      object ToolButtonShowText: TToolButton
        Left = 0
        Top = 0
        Cursor = crHandPoint
        AutoSize = True
        Caption = 'Hide text'
        ImageIndex = 1
        ImageName = 'Hide-Text'
        Visible = False
        OnClick = ToolButtonShowTextClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonZoomIn: TToolButton
        Left = 85
        Top = 0
        Cursor = crHandPoint
        Hint = 'Zoom in (increase font size)'
        AutoSize = True
        Caption = 'Zoom In'
        ImageIndex = 6
        ImageName = 'plus'
        OnClick = ToolButtonZoomInClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonZoomOut: TToolButton
        Left = 164
        Top = 0
        Cursor = crHandPoint
        Hint = 'Zoom out (decrease font size)'
        AutoSize = True
        Caption = 'Zoom Out'
        ImageIndex = 7
        ImageName = 'minus'
        OnClick = ToolButtonZoomOutClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonSettings: TToolButton
        Left = 239
        Top = 0
        Cursor = crHandPoint
        Hint = 'Preview settings...'
        AutoSize = True
        Caption = 'Settings...'
        ImageIndex = 12
        ImageName = 'preferences-desktop'
        Visible = False
        OnClick = ToolButtonSettingsClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
      object ToolButtonAbout: TToolButton
        Left = 329
        Top = 0
        Cursor = crHandPoint
        Hint = 'Show about...'
        AutoSize = True
        Caption = 'About...'
        ImageIndex = 2
        ImageName = 'about'
        Visible = False
        OnClick = ToolButtonAboutClick
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
      end
    end
  end
  object PanelMD: TPanel
    Left = 0
    Top = 70
    Width = 617
    Height = 110
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    ExplicitWidth = 656
    object SynEdit: TSynEdit
      Left = 0
      Top = 0
      Width = 656
      Height = 110
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Pitch = fpFixed
      Font.Style = []
      TabOrder = 0
      CodeFolding.GutterShapeSize = 11
      CodeFolding.CollapsedLineColor = clGrayText
      CodeFolding.FolderBarLinesColor = clGrayText
      CodeFolding.IndentGuidesColor = clGray
      CodeFolding.IndentGuides = True
      CodeFolding.ShowCollapsedLine = False
      CodeFolding.ShowHintMark = True
      UseCodeFolding = False
      BorderStyle = bsNone
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
      ReadOnly = True
      FontSmoothing = fsmNone
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 596
    Width = 617
    Height = 21
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = ' Mardown file Preview - Copyright '#169' 2021-2022 - Ethea S.r.l.'
    SizeGrip = False
    UseSystemFont = False
    ExplicitTop = 629
    ExplicitWidth = 656
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 566
    Width = 617
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 599
    ExplicitWidth = 656
    object ToolBarAllegati: TToolBar
      Left = 0
      Top = 0
      Width = 656
      Height = 30
      Align = alClient
      AutoSize = True
      ButtonHeight = 30
      ButtonWidth = 81
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = SVGIconImageList
      List = True
      ShowCaptions = True
      TabOrder = 0
      Transparent = True
    end
  end
  object HtmlViewer: THtmlViewer
    Left = 0
    Top = 186
    Width = 617
    Height = 380
    BorderStyle = htFocused
    HistoryMaxCount = 0
    NoSelect = False
    PrintMarginBottom = 2.000000000000000000
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintScale = 1.000000000000000000
    ScrollBars = ssVertical
    Text = ''
    Align = alClient
    TabOrder = 4
    Touch.InteractiveGestures = [igPan]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia]
    ExplicitWidth = 656
    ExplicitHeight = 413
  end
  object paTop: TPanel
    Left = 0
    Top = 35
    Width = 617
    Height = 35
    Align = alTop
    TabOrder = 5
    ExplicitWidth = 656
    object ProcessorDialectLabel: TLabel
      Left = 8
      Top = 11
      Width = 55
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Dialect:'
    end
    object ProcessorDialectComboBox: TComboBox
      Left = 69
      Top = 8
      Width = 170
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnSelect = ProcessorDialectComboBoxSelect
      Items.Strings = (
        'DaringFireball'
        'CommonMark')
    end
  end
  object SVGIconImageList: TVirtualImageList
    Images = <
      item
        CollectionIndex = 42
        CollectionName = 'Show-Text'
        Name = 'Show-Text'
      end
      item
        CollectionIndex = 43
        CollectionName = 'Hide-Text'
        Name = 'Hide-Text'
      end
      item
        CollectionIndex = 23
        CollectionName = 'about'
        Name = 'about'
      end
      item
        CollectionIndex = 41
        CollectionName = 'Support'
        Name = 'Support'
      end
      item
        CollectionIndex = 0
        CollectionName = 'Style'
        Name = 'Style'
      end
      item
        CollectionIndex = 45
        CollectionName = 'Services'
        Name = 'Services'
      end
      item
        CollectionIndex = 26
        CollectionName = 'plus'
        Name = 'plus'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Minus'
        Name = 'Minus'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Search'
        Name = 'Search'
      end
      item
        CollectionIndex = 38
        CollectionName = 'export'
        Name = 'export'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Reformat'
        Name = 'Reformat'
      end
      item
        CollectionIndex = 48
        CollectionName = 'attachment'
        Name = 'attachment'
      end
      item
        CollectionIndex = 28
        CollectionName = 'preferences-desktop'
        Name = 'preferences-desktop'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Print-preview'
        Name = 'Print-preview'
      end>
    ImageCollection = dmResources.SVGIconImageCollection
    Width = 24
    Height = 24
    Left = 384
    Top = 208
  end
end
