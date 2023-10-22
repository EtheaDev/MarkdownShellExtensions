object InputUrlDialog: TInputUrlDialog
  Left = 0
  Top = 0
  Caption = 'Insert'
  ClientHeight = 362
  ClientWidth = 608
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 528
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  TextHeight = 15
  object paButton: TPanel
    Left = 0
    Top = 319
    Width = 608
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btConferma: TStyledButton
      AlignWithMargins = True
      Left = 446
      Top = 10
      Width = 75
      Height = 23
      Margins.Top = 10
      Margins.Bottom = 10
      Action = acConfirm
      Align = alRight
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btAnnulla: TStyledButton
      AlignWithMargins = True
      Left = 527
      Top = 10
      Width = 75
      Height = 23
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Action = acCancel
      Align = alRight
      Caption = '&Cancel'
      Cancel = True
      ModalResult = 2
      TabOrder = 1
    end
  end
  object paEdit: TPanel
    Left = 0
    Top = 57
    Width = 320
    Height = 262
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object lbText: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 300
      Height = 15
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Align = alTop
      Caption = 'Alternative Text'
      ExplicitWidth = 81
    end
    object edText: TEdit
      AlignWithMargins = True
      Left = 10
      Top = 28
      Width = 300
      Height = 23
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Margins.Bottom = 0
      Align = alTop
      TabOrder = 0
    end
  end
  object paImage: TPanel
    AlignWithMargins = True
    Left = 322
    Top = 64
    Width = 276
    Height = 253
    Margins.Left = 2
    Margins.Top = 7
    Margins.Right = 10
    Margins.Bottom = 2
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 3
    object Image: TImage
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 272
      Height = 246
      Hint = 'Anteprima dell'#39'immagine'
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
    end
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 608
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      608
      57)
    object lbUrl: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 386
      Height = 15
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 212
      Align = alTop
      Caption = 'Filename or URL'
      ExplicitWidth = 86
    end
    object edUrl: TEdit
      AlignWithMargins = True
      Left = 10
      Top = 28
      Width = 388
      Height = 23
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 210
      Margins.Bottom = 0
      Align = alTop
      TabOrder = 0
      OnChange = edUrlChange
    end
    object btOpenDlg: TStyledButton
      Left = 414
      Top = 9
      Width = 87
      Height = 45
      Action = acOpenDlg
      Anchors = [akTop, akRight]
      ImageAlignment = iaTop
      Images = VirtualImageList
      ImageName = 'Open'
      TabOrder = 2
    end
    object btPaste: TStyledButton
      Left = 507
      Top = 9
      Width = 87
      Height = 45
      Action = acPaste
      Anchors = [akTop, akRight]
      ImageAlignment = iaTop
      Images = VirtualImageList
      ImageName = 'globe'
      TabOrder = 1
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All (*.*)|*.*|MarkDown File (*.md)|*.md|PDF File (*.pdf)|*.pdf'
    Left = 168
    Top = 120
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 56
    Top = 120
  end
  object ActionList: TActionList
    Images = VirtualImageList
    Left = 168
    Top = 176
    object acPaste: TAction
      Caption = '&Paste URL'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 1
      ImageName = 'globe'
      OnExecute = acPasteExecute
      OnUpdate = acPasteUpdate
    end
    object acOpenDlg: TAction
      Caption = '&Local File'
      Hint = 'Select a file from disk'
      ImageIndex = 0
      ImageName = 'Open'
      OnExecute = acOpenDlgExecute
    end
    object acOpenPictureDlg: TAction
      Caption = '&Local Image'
      Hint = 'Select an image from disc'
      ImageIndex = 0
      ImageName = 'Open'
      OnExecute = acOpenPictureDlgExecute
    end
    object acConfirm: TAction
      Caption = '&Conferma'
      OnExecute = acConfirmExecute
    end
    object acCancel: TAction
      Caption = '&Annulla'
      OnExecute = acCancelExecute
    end
  end
  object VirtualImageList: TVirtualImageList
    Images = <
      item
        CollectionIndex = 1
        CollectionName = 'Open'
        Name = 'Open'
      end
      item
        CollectionIndex = 76
        CollectionName = 'globe'
        Name = 'globe'
      end>
    ImageCollection = dmResources.SVGIconImageCollection
    Left = 56
    Top = 176
  end
end
