object frmMain: TfrmMain
  Left = 302
  Top = 219
  Width = 680
  Height = 592
  Caption = 'Server Socket Communication'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    664
    554)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 56
    Height = 13
    Caption = 'Server Port:'
  end
  object edtServerPort: TEdit
    Left = 16
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 1
    Text = '2636'
  end
  object btnServerListen: TButton
    Left = 104
    Top = 22
    Width = 97
    Height = 25
    Caption = 'Active Server'
    TabOrder = 0
    OnClick = btnServerListenClick
  end
  object mmoLog: TMemo
    Left = 8
    Top = 72
    Width = 645
    Height = 478
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
end
