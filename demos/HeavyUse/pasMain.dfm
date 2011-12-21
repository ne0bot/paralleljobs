object frmMain: TfrmMain
  Left = 405
  Top = 334
  BorderStyle = bsDialog
  Caption = 'Heavy Use'
  ClientHeight = 139
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblProcess: TLabel
    Left = 32
    Top = 88
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label1: TLabel
    Left = 32
    Top = 24
    Width = 176
    Height = 13
    Caption = 'Use count: (Threads - safe limit 1800)'
  end
  object lblCalls: TLabel
    Left = 32
    Top = 72
    Width = 9
    Height = 13
    Caption = '...'
  end
  object edtTestCount: TEdit
    Left = 32
    Top = 40
    Width = 177
    Height = 21
    TabOrder = 1
    Text = '1024'
  end
  object btnTest: TButton
    Left = 216
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object tmrCounterCheck: TTimer
    Interval = 100
    OnTimer = tmrCounterCheckTimer
    Left = 224
    Top = 96
  end
end
