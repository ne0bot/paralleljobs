object frmMain: TfrmMain
  Left = 302
  Top = 219
  Width = 590
  Height = 241
  Caption = 'Index Searching :: http://code.google.com/p/paralleljobs/'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblSize: TLabel
    Left = 68
    Top = 12
    Width = 42
    Height = 13
    Caption = '5000000'
  end
  object Label1: TLabel
    Left = 20
    Top = 12
    Width = 42
    Height = 13
    Caption = 'List Size:'
  end
  object lblTimer: TLabel
    Left = 102
    Top = 132
    Width = 377
    Height = 61
    Alignment = taCenter
    AutoSize = False
    BiDiMode = bdLeftToRight
    Caption = '0'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentBiDiMode = False
    ParentFont = False
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 120
    Top = 76
    Width = 42
    Height = 13
    Caption = 'Threads:'
  end
  object lblThreads: TLabel
    Left = 168
    Top = 76
    Width = 6
    Height = 13
    Caption = '1'
  end
  object tckSize: TTrackBar
    Left = 20
    Top = 32
    Width = 545
    Height = 29
    Max = 10000000
    Min = 100
    Position = 5000000
    TabOrder = 0
    ThumbLength = 10
    OnChange = tckSizeChange
  end
  object btnSearch: TButton
    Left = 394
    Top = 96
    Width = 75
    Height = 21
    Caption = 'Search'
    TabOrder = 4
    OnClick = btnSearchClick
  end
  object edtSearchItem: TEdit
    Left = 258
    Top = 96
    Width = 83
    Height = 21
    TabOrder = 2
    Text = '4305242'
  end
  object tckThreads: TTrackBar
    Left = 120
    Top = 96
    Width = 105
    Height = 21
    Min = 1
    Position = 1
    TabOrder = 1
    ThumbLength = 10
    OnChange = tckThreadsChange
  end
  object btnRand: TButton
    Left = 344
    Top = 96
    Width = 21
    Height = 21
    Caption = 'P'
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Wingdings 3'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = btnRandClick
  end
end
