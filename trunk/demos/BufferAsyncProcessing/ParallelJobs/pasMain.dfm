object frmMain: TfrmMain
  Left = 302
  Top = 219
  Width = 590
  Height = 340
  Caption = 
    'Buffer Async Processing :: http://code.google.com/p/paralleljobs' +
    '/'
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
    574
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object edtCreateCount: TEdit
    Left = 24
    Top = 24
    Width = 81
    Height = 21
    TabOrder = 0
    Text = '10'
  end
  object btnCreate: TButton
    Left = 112
    Top = 24
    Width = 89
    Height = 25
    Caption = 'Create Buffers'
    TabOrder = 1
    OnClick = btnCreateClick
  end
  object mmoLog: TMemo
    Left = 16
    Top = 64
    Width = 537
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
end
