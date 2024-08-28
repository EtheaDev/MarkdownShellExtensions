inherited FrmPreview: TFrmPreview
  Left = 522
  Top = 286
  ClientHeight = 616
  ClientWidth = 613
  DoubleBuffered = True
  Font.Name = 'Segoe UI'
  OnResize = FormResize
  ExplicitWidth = 629
  ExplicitHeight = 655
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 145
    Width = 613
    Height = 6
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    MinSize = 100
    OnMoved = SplitterMoved
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 613
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object StyledToolBar: TStyledToolbar
      Left = 0
      Top = 0
      Width = 613
      Height = 35
      Align = alClient
      AutoSize = True
      ButtonHeight = 30
      ButtonWidth = 87
      Images = SVGIconImageList
      List = True
      ShowCaptions = True
      TabOrder = 0
      object ToolButtonShowText: TStyledToolButton
        Left = 0
        Top = 0
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonShowTextClick
        Visible = False
        Caption = 'Hide text'
        ImageIndex = 1
        ImageName = 'Hide-Text'
      end
      object ToolButtonZoomIn: TStyledToolButton
        Left = 87
        Top = 0
        Hint = 'Zoom in (increase font size)'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonZoomInClick
        Caption = 'Zoom In'
        ImageIndex = 6
        ImageName = 'plus'
      end
      object ToolButtonZoomOut: TStyledToolButton
        Left = 174
        Top = 0
        Hint = 'Zoom out (decrease font size)'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonZoomOutClick
        Caption = 'Zoom Out'
        ImageIndex = 7
        ImageName = 'minus'
      end
      object ToolButtonSettings: TStyledToolButton
        Left = 261
        Top = 0
        Hint = 'Preview settings...'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonSettingsClick
        Visible = False
        Caption = 'Settings...'
        ImageIndex = 12
        ImageName = 'preferences-desktop'
      end
      object ToolButtonAbout: TStyledToolButton
        Left = 348
        Top = 0
        Hint = 'Show about...'
        OnMouseEnter = ToolButtonMouseEnter
        OnMouseLeave = ToolButtonMouseLeave
        OnClick = ToolButtonAboutClick
        Visible = False
        Caption = 'About...'
        ImageIndex = 2
        ImageName = 'about'
      end
    end
  end
  object PanelMD: TPanel
    Left = 0
    Top = 35
    Width = 613
    Height = 110
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object SynEdit: TSynEdit
      Left = 0
      Top = 0
      Width = 613
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
    Top = 595
    Width = 613
    Height = 21
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = ' Markdown file Preview - Copyright '#169' 2021-2024 - Ethea S.r.l.'
    SizeGrip = False
    UseSystemFont = False
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 565
    Width = 613
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ToolBarAllegati: TStyledToolbar
      Left = 0
      Top = 0
      Width = 613
      Height = 30
      Align = alClient
      AutoSize = True
      ButtonHeight = 30
      ButtonWidth = 81
      Images = SVGIconImageList
      List = True
      ShowCaptions = True
      TabOrder = 0
    end
  end
  object HtmlViewer: THtmlViewer
    Left = 0
    Top = 151
    Width = 613
    Height = 414
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
