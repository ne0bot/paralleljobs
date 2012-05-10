object frmMain: TfrmMain
  Left = 257
  Top = 297
  Width = 676
  Height = 378
  Caption = 'AppTest'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    660
    340)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTArea: TPanel
    Left = 184
    Top = 88
    Width = 481
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    OnResize = pnlTAreaResize
    DesignSize = (
      481
      257)
    object mmoT1: TMemo
      Left = 0
      Top = 0
      Width = 113
      Height = 257
      Anchors = [akLeft, akTop, akBottom]
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object mmoT2: TMemo
      Left = 120
      Top = 0
      Width = 113
      Height = 257
      Anchors = [akLeft, akTop, akBottom]
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object mmoT3: TMemo
      Left = 240
      Top = 0
      Width = 113
      Height = 257
      Anchors = [akLeft, akTop, akBottom]
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object mmoT4: TMemo
      Left = 360
      Top = 0
      Width = 113
      Height = 257
      Anchors = [akLeft, akTop, akBottom]
      ScrollBars = ssVertical
      TabOrder = 3
    end
  end
  object btnJob1: TButton
    Left = 8
    Top = 16
    Width = 165
    Height = 25
    Caption = 'Parallel Job Test #1'
    TabOrder = 1
    OnClick = btnJob1Click
  end
  object mmoDisplay: TMemo
    Left = 184
    Top = 8
    Width = 481
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnJob2: TButton
    Left = 8
    Top = 48
    Width = 165
    Height = 25
    Caption = 'Parallel Job Test #2'
    TabOrder = 2
    OnClick = btnJob2Click
  end
  object btnGroupJobs: TButton
    Left = 8
    Top = 92
    Width = 165
    Height = 25
    Caption = 'Parallel Job Test #3 [Groups]'
    TabOrder = 4
    OnClick = btnGroupJobsClick
  end
  object btnStopTest3: TButton
    Left = 52
    Top = 124
    Width = 121
    Height = 25
    Caption = 'Stop Test #3 [Groups]'
    TabOrder = 5
    OnClick = btnStopTest3Click
  end
  object btn1: TButton
    Left = 8
    Top = 176
    Width = 75
    Height = 25
    Caption = 'End All Jobs'
    TabOrder = 6
    OnClick = btn1Click
  end
end
